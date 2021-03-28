/* NeXT/Open/GNUstep and macOS Cocoa menu and toolbar module.
   Copyright (C) 2007-2017 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

/*
By Adrian Robert, based on code from original nsmenu.m (Carl Edman,
Christian Limpach, Scott Bender, Christophe de Dinechin) and code in the
Carbon version by Yamamoto Mitsuharu. */

/* This should be the first include, as it may set up #defines affecting
   interpretation of even the system includes. */
#include <config.h>

#include "lisp.h"
#include "window.h"
#include "character.h"
#include "buffer.h"
#include "keymap.h"
#include "coding.h"
#include "commands.h"
#include "blockinput.h"
#include "nsterm.h"
#include "termhooks.h"
#include "keyboard.h"
#include "menu.h"

#define NSMENUPROFILE 0

#if NSMENUPROFILE
#include <sys/timeb.h>
#include <sys/types.h>
#endif


#if 0
/* Include lisp -> C common menu parsing code */
#define ENCODE_MENU_STRING(str) ENCODE_UTF_8 (str)
#include "nsmenu_common.c"
#endif

Lisp_Object Vcancel_special_indicator_flag;

extern long context_menu_value;
EmacsMenu *mainMenu, *svcsMenu, *dockMenu;
NSMenu *panelMenu;

/* Nonzero means a menu is currently active.  */
static int popup_activated_flag;
static NSModalSession popupSession;
static EmacsAlertPanel *popupSheetAlert;

/* Nonzero means we are tracking and updating menus.  */
static int trackingMenu;


/* NOTE: toolbar implementation is at end,
  following complete menu implementation. */


/* ==========================================================================

    Menu: Externally-called functions

   ========================================================================== */


/* Supposed to discard menubar and free storage.  Since we share the
   menubar among frames and update its context for the focused window,
   there is nothing to do here. */
void
free_frame_menubar (struct frame *f)
{
  return;
}


int
popup_activated (void)
{
  return popup_activated_flag;
}

/* --------------------------------------------------------------------------
    Update menubar.  Three cases:
    1) ! deep_p, submenu = nil: Fresh switch onto a frame -- either set up
       just top-level menu strings (macOS), or goto case (2) (GNUstep).
    2) deep_p, submenu = nil: Recompute all submenus.
    3) deep_p, submenu = non-nil: Update contents of a single submenu.
   -------------------------------------------------------------------------- */
static
void
ns_update_menubar (struct frame *f, bool deep_p, EmacsMenu *submenu)
{
  NSMenu *app_menu = [NSApp mainMenu];
  EmacsMenu *menu;
  static EmacsMenu *last_submenu = nil;
  BOOL needsSet = NO;
  bool owfi;
  Lisp_Object items;
  widget_value *wv, *first_wv, *prev_wv = 0;
  int i;

#if NSMENUPROFILE
  struct timeb tb;
  long t;
#endif

  NSTRACE ("ns_update_menubar");

  if (f != SELECTED_FRAME ())
      return;
  if ([[FRAME_NS_VIEW (f) window] attachedSheet] &&
      [[[FRAME_NS_VIEW (f) window] attachedSheet] isKindOfClass: [EmacsSavePanel class]])
    {
      //[mainMenu retain];
      //[NSApp setMainMenu: [panelMenu retain]];
      [NSApp setMainMenu: panelMenu];
      return;
    }
  if (! [[FRAME_NS_VIEW (f) window] isKindOfClass: [EmacsWindow class]])
    {
      return;
    }

  [NSApp setMainMenu: mainMenu];

  XSETFRAME (Vmenu_updating_frame, f);
/*fprintf (stderr, "ns_update_menubar: frame: %p\tdeep: %d\tsub: %p\n", f, deep_p, submenu); */

  block_input ();

  /* Menu may have been created automatically; if so, discard it. */
  if ([app_menu isKindOfClass: [EmacsMenu class]] == NO) // && menu != panelMenu)
    {
      if (app_menu != panelMenu)
        [app_menu release];
      menu = nil;
    }
  else
    {
      menu = (EmacsMenu *) app_menu;
    }

  if (menu == nil)
    {
      menu = [[EmacsMenu alloc] initWithTitle: ns_app_name];
      needsSet = YES;
    }

#if NSMENUPROFILE
  ftime (&tb);
  t = -(1000*tb.time+tb.millitm);
#endif

#ifdef NS_IMPL_GNUSTEP
  deep_p = 1; /* until GNUstep NSMenu implements the Panther delegation model */
#endif
  if (deep_p)
    {
      /* Fully parse one or more of the submenus. */
      int n = 0;
      int *submenu_start, *submenu_end;
      bool *submenu_top_level_items;
      int *submenu_n_panes;
      struct buffer *prev = current_buffer;
      Lisp_Object buffer;
      ptrdiff_t specpdl_count = SPECPDL_INDEX ();
      int previous_menu_items_used = f->menu_bar_items_used;
      Lisp_Object *previous_items
	= alloca (previous_menu_items_used * sizeof *previous_items);

      /* lisp preliminaries */
      buffer = XWINDOW (FRAME_SELECTED_WINDOW (f))->contents;
      specbind (Qinhibit_quit, Qt);
      specbind (Qdebug_on_next_call, Qnil);
      record_unwind_save_match_data ();
      if (NILP (Voverriding_local_map_menu_flag))
	{
	  specbind (Qoverriding_terminal_local_map, Qnil);
	  specbind (Qoverriding_local_map, Qnil);
	}
      set_buffer_internal_1 (XBUFFER (buffer));

      /* TODO: for some reason this is not needed in other terms,
           but some menu updates call Info-extract-pointer which causes
           abort-on-error if waiting-for-input.  Needs further investigation. */
      owfi = waiting_for_input;
      waiting_for_input = 0;

      /* lucid hook and possible reset */
      safe_run_hooks (Qactivate_menubar_hook);
      if (! NILP (Vlucid_menu_bar_dirty_flag))
	call0 (Qrecompute_lucid_menubar);
      safe_run_hooks (Qmenu_bar_update_hook);
      fset_menu_bar_items (f, menu_bar_items (FRAME_MENU_BAR_ITEMS (f)));

      /* Now ready to go */
      items = FRAME_MENU_BAR_ITEMS (f);

      /* Save the frame's previous menu bar contents data */
      if (previous_menu_items_used)
	memcpy (previous_items, aref_addr (f->menu_bar_vector, 0),
		previous_menu_items_used * sizeof (Lisp_Object));

      /* parse stage 1: extract from lisp */
      save_menu_items ();

      menu_items = f->menu_bar_vector;
      menu_items_allocated = VECTORP (menu_items) ? ASIZE (menu_items) : 0;
      submenu_start = alloca (ASIZE (items) * sizeof *submenu_start);
      submenu_end = alloca (ASIZE (items) * sizeof *submenu_end);
      submenu_n_panes = alloca (ASIZE (items) * sizeof *submenu_n_panes);
      submenu_top_level_items = alloca (ASIZE (items)
					* sizeof *submenu_top_level_items);
      init_menu_items ();
      for (i = 0; i < ASIZE (items); i += 4)
	{
	  Lisp_Object key, string, maps;

	  key = AREF (items, i);
	  string = AREF (items, i + 1);
	  maps = AREF (items, i + 2);
	  if (NILP (string))
	    break;

          /* FIXME: we'd like to only parse the needed submenu, but this
               was causing crashes in the _common parsing code.. need to make
               sure proper initialization done.. */
/*        if (submenu && strcmp ([[submenu title] UTF8String], SSDATA (string)))
             continue; */

	  submenu_start[i] = menu_items_used;

	  menu_items_n_panes = 0;
	  submenu_top_level_items[i] = parse_single_submenu (key, string, maps);
	  submenu_n_panes[i] = menu_items_n_panes;
	  submenu_end[i] = menu_items_used;
          n++;
	}

      finish_menu_items ();
      waiting_for_input = owfi;


      if (submenu && n == 0)
        {
          /* should have found a menu for this one but didn't */
          fprintf (stderr, "ERROR: did not find lisp menu for submenu '%s'.\n",
                  [[submenu title] UTF8String]);
	  discard_menu_items ();
	  unbind_to (specpdl_count, Qnil);
          unblock_input ();
	  return;
        }

      /* parse stage 2: insert into lucid 'widget_value' structures
         [comments in other terms say not to evaluate lisp code here] */
      wv = make_widget_value ("menubar", NULL, true, Qnil);
      wv->button_type = BUTTON_TYPE_NONE;
      first_wv = wv;

      for (i = 0; i < 4*n; i += 4)
	{
	  menu_items_n_panes = submenu_n_panes[i];
	  wv = digest_single_submenu (submenu_start[i], submenu_end[i],
				      submenu_top_level_items[i]);
	  if (prev_wv)
	    prev_wv->next = wv;
	  else
	    first_wv->contents = wv;
	  /* Don't set wv->name here; GC during the loop might relocate it.  */
	  wv->enabled = 1;
	  wv->button_type = BUTTON_TYPE_NONE;
	  prev_wv = wv;
	}

      set_buffer_internal_1 (prev);

      /* Compare the new menu items with previous, and leave off if no change */
      /* FIXME: following other terms here, but seems like this should be
           done before parse stage 2 above, since its results aren't used */
      if (previous_menu_items_used
          && (!submenu || (submenu && submenu == last_submenu))
          && menu_items_used == previous_menu_items_used)
        {
          for (i = 0; i < previous_menu_items_used; i++)
            /* FIXME: this ALWAYS fails on Buffers menu items.. something
                 about their strings causes them to change every time, so we
                 double-check failures */
            if (!EQ (previous_items[i], AREF (menu_items, i)))
              if (!(STRINGP (previous_items[i])
                    && STRINGP (AREF (menu_items, i))
                    && !strcmp (SSDATA (previous_items[i]),
				SSDATA (AREF (menu_items, i)))))
                  break;
          if (i == previous_menu_items_used)
            {
              /* No change.. */

#if NSMENUPROFILE
              ftime (&tb);
              t += 1000*tb.time+tb.millitm;
              fprintf (stderr, "NO CHANGE!  CUTTING OUT after %ld msec.\n", t);
#endif

              free_menubar_widget_value_tree (first_wv);
              discard_menu_items ();
              unbind_to (specpdl_count, Qnil);
              unblock_input ();
              return;
            }
        }
      /* The menu items are different, so store them in the frame */
      /* FIXME: this is not correct for single-submenu case */
      fset_menu_bar_vector (f, menu_items);
      f->menu_bar_items_used = menu_items_used;

      /* Calls restore_menu_items, etc., as they were outside */
      unbind_to (specpdl_count, Qnil);

      /* Parse stage 2a: now GC cannot happen during the lifetime of the
         widget_value, so it's safe to store data from a Lisp_String */
      wv = first_wv->contents;
      for (i = 0; i < ASIZE (items); i += 4)
	{
	  Lisp_Object string;
	  string = AREF (items, i + 1);
	  if (NILP (string))
	    break;

	  wv->name = SSDATA (string);
          update_submenu_strings (wv->contents);
	  wv = wv->next;
	}

      /* Now, update the NS menu; if we have a submenu, use that, otherwise
         create a new menu for each sub and fill it. */
      if (submenu)
        {
          const char *submenuTitle = [[submenu title] UTF8String];
          for (wv = first_wv->contents; wv; wv = wv->next)
            {
              if (!strcmp (submenuTitle, wv->name))
                {
                  [submenu fillWithWidgetValue: wv->contents];
                  last_submenu = submenu;
                  break;
                }
            }
        }
      else
        {
          [menu fillWithWidgetValue: first_wv->contents frame: f];
        }

    }
  else
    {
      static int n_previous_strings = 0;
      static char previous_strings[100][10];
      static struct frame *last_f = NULL;
      int n;
      Lisp_Object string;

      wv = make_widget_value ("menubar", NULL, true, Qnil);
      wv->button_type = BUTTON_TYPE_NONE;
      first_wv = wv;

      /* Make widget-value tree w/ just the top level menu bar strings */
      items = FRAME_MENU_BAR_ITEMS (f);
      if (NILP (items))
        {
          free_menubar_widget_value_tree (first_wv);
          unblock_input ();
          return;
        }


      /* check if no change.. this mechanism is a bit rough, but ready */
      n = ASIZE (items) / 4;
      if (f == last_f && n_previous_strings == n)
        {
          for (i = 0; i<n; i++)
            {
	      string = AREF (items, 4*i+1);

              if (EQ (string, make_number (0))) // FIXME: Why???  --Stef
                continue;
              if (NILP (string))
                {
                  if (previous_strings[i][0])
                    break;
                  else
                    continue;
                }
              else if (memcmp (previous_strings[i], SDATA (string),
			  min (10, SBYTES (string) + 1)))
                break;
            }

          if (i == n)
            {
              free_menubar_widget_value_tree (first_wv);
              unblock_input ();
              return;
            }
        }

      [menu clear];
      for (i = 0; i < ASIZE (items); i += 4)
	{
	  string = AREF (items, i + 1);
	  if (NILP (string))
	    break;

          if (n < 100)
	    memcpy (previous_strings[i/4], SDATA (string),
                    min (10, SBYTES (string) + 1));

	  wv = make_widget_value (SSDATA (string), NULL, true, Qnil);
	  wv->button_type = BUTTON_TYPE_NONE;
	  wv->call_data = (void *) (intptr_t) (-1);

#ifdef NS_IMPL_COCOA
          /* we'll update the real copy under app menu when time comes */
          if (!strcmp ("Services", wv->name))
            {
              /* but we need to make sure it will update on demand */
              [svcsMenu setFrame: f];
            }
          else
#endif
          [menu addSubmenuWithTitle: wv->name forFrame: f];

	  if (prev_wv)
              prev_wv->next = wv;
	  else
              first_wv->contents = wv;
	  prev_wv = wv;
	}

      last_f = f;
      if (n < 100)
        n_previous_strings = n;
      else
        n_previous_strings = 0;

    }
  free_menubar_widget_value_tree (first_wv);


#if NSMENUPROFILE
  ftime (&tb);
  t += 1000*tb.time+tb.millitm;
  fprintf (stderr, "Menu update took %ld msec.\n", t);
#endif

  /* set main menu */
  if (needsSet)
    [NSApp setMainMenu: menu];

  unblock_input ();

}


/* Main emacs core entry point for menubar menus: called to indicate that the
   frame's menus have changed, and the *step representation should be updated
   from Lisp. */
void
set_frame_menubar (struct frame *f, bool first_time, bool deep_p)
{
  ns_update_menubar (f, deep_p, nil);
}

void
x_activate_menubar (struct frame *f)
{
#ifdef NS_IMPL_COCOA
  ns_update_menubar (f, true, nil);
  ns_check_pending_open_menu ();
#endif
}
/* Utility (from macmenu.c): is this item a separator? */
static int
name_is_separator (name)
     const char *name;
{
  const char *start = name;

  /* Check if name string consists of only dashes ('-').  */
  while (*name == '-') name++;
  /* Separators can also be of the form "--:TripleSuperMegaEtched"
     or "--deep-shadow".  We don't implement them yet, se we just treat
     them like normal separators.  */
  return (*name == '\0' || start + 2 == name);
}


/* ==========================================================================

    Menu: class implementation

   ========================================================================== */


/* Menu that can define itself from Emacs "widget_value"s and will lazily
   update itself when user clicked.  Based on Carbon/AppKit implementation
   by Yamamoto Mitsuharu. */
@implementation EmacsMenu

/* override designated initializer */
- initWithTitle: (NSString *)title
{
  frame = 0;
  if ((self = [super initWithTitle: title]))
    [self setAutoenablesItems: NO];
  if (NILP (Vns_menu_display_services))
    [self setAllowsContextMenuPlugIns: NO];
  return self;
}


/* used for top-level */
- initWithTitle: (NSString *)title frame: (struct frame *)f
{
  [self initWithTitle: title];
  frame = f;
#ifdef NS_IMPL_COCOA
  [self setDelegate: self];
#endif
  return self;
}


- (void)setFrame: (struct frame *)f
{
  frame = f;
}

#ifdef NS_IMPL_COCOA
-(void)trackingNotification:(NSNotification *)notification
{
  /* Update menu in menuNeedsUpdate only while tracking menus.  */
  trackingMenu = ([notification name] == NSMenuDidBeginTrackingNotification
                  ? 1 : 0);
  if (! trackingMenu) ns_check_menu_open (nil);
}

- (void)menuWillOpen:(NSMenu *)menu
{
  ++trackingMenu;

#if MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_7
  // On 10.6 we get repeated calls, only the one for NSSystemDefined is "real".
  if ([[NSApp currentEvent] type] != NSSystemDefined) return;
#endif

  /* When dragging from one menu to another, we get willOpen followed by didClose,
     i.e. trackingMenu == 3 in willOpen and then 2 after didClose.
     We have updated all menus, so avoid doing it when trackingMenu == 3.  */
  if (trackingMenu == 2)
    ns_check_menu_open (menu);
}

- (void)menuDidClose:(NSMenu *)menu
{
  --trackingMenu;
}

#endif /* NS_IMPL_COCOA */

/* delegate method called when a submenu is being opened: run a 'deep' call
   to set_frame_menubar */
- (void)menuNeedsUpdate: (NSMenu *)menu
{
  /* events may be sent to recently deleted frames,
     FRAME_NS_VIEW (frame) returns 0x0 */
  if (! (frame && FRAME_LIVE_P (frame) && FRAME_NS_VIEW (frame)))
    return;

  /* Cocoa/Carbon will request update on every keystroke
     via IsMenuKeyEvent -> CheckMenusForKeyEvent.  These are not needed
     since key equivalents are handled through emacs.
     On Leopard, even keystroke events generate SystemDefined event.
     Third-party applications that enhance mouse / trackpad
     interaction, or also VNC/Remote Desktop will send events
     of type AppDefined rather than SysDefined.
     Menus will fail to show up if they haven't been initialized.
     AppDefined events may lack timing data.

     Thus, we rely on the didBeginTrackingNotification notification
     as above to indicate the need for updates.
     From 10.6 on, we could also use -[NSMenu propertiesToUpdate]: In the
     key press case, NSMenuPropertyItemImage (e.g.) won't be set.
  */
  if (trackingMenu == 0)
    return;
/*fprintf (stderr, "Updating menu '%s'\n", [[self title] UTF8String]); NSLog (@"%@\n", event); */
#ifdef NS_IMPL_GNUSTEP
  /* Don't know how to do this for anything other than Mac OS X 10.5 and later.
     This is wrong, as it might run Lisp code in the event loop.  */
  ns_update_menubar (frame, true, self);
#endif
}


- (BOOL)performKeyEquivalent: (NSEvent *)event
{
  if (SELECTED_FRAME () && FRAME_NS_P (SELECTED_FRAME ())
      && FRAME_NS_VIEW (SELECTED_FRAME ())
      /* must check if EmacsWindow.  Could be sheet/NSPanel.
         If on space without a window, [event window] is nil. */
       && (! [event window] || [[event window] isKindOfClass: [EmacsWindow class]]))
     {
       /* if we don't have a frame as key,
	  then the selected frame/window and current buffer will be used.
	  To Do: prevent changes to that buffer.
       (setting read-only here doesn't help - the event is processed later.)*/
       [FRAME_NS_VIEW (SELECTED_FRAME ()) keyDown: event];
     }
  else
    {
      /* open panels (text fields, etc.) require a
        menu with Edit submenu, containing Copy, Paste, Undo, etc. functions
        that send the correct actions to the first responders.
        Therefore, the panelMenu (which is the main menu normally when sheets
        are shown) is called. */
      return NO;
      // return [[NSApp mainMenu] performKeyEquivalent:event];
    }

  return YES;
}


/* Parse a widget_value's key rep (examples: 's-p', 's-S', '(C-x C-s)', '<f13>')
   into an accelerator string.  We are only able to display a single character
   for an accelerator, together with an optional modifier combination.  (Under
   Carbon more control was possible, but in Cocoa multi-char strings passed to
   NSMenuItem get ignored.  For now we try to display a super-single letter
   combo, and return the others as strings to be appended to the item title.
   (This is signaled by setting keyEquivModMask to 0 for now.) */
-(NSString *)parseKeyEquiv: (const char *)key
{
  const char *tpos = key;
  keyEquivModMask = NSCommandKeyMask;

  if (!key || !strlen (key))
    return @"";

  while (*tpos == ' ' || *tpos == '(')
    tpos++;

#ifndef NS_IMPL_COCOA
  if (*tpos != 's')
#endif
    {
  keyEquivModMask = 0; /* signal */
  return [NSString stringWithUTF8String: tpos];
}
  return [NSString stringWithFormat: @"%c", tpos[2]];
}

- (NSMenuItem *)addItemWithWidgetValue: (void *)wvptr
{
  NSMenuItem *item;
  widget_value *wv = (widget_value *)wvptr;

  if (menu_separator_name_p (wv->name))
    {
      item = [NSMenuItem separatorItem];
      [self addItem: item];
    }
  else
    {
      NSString *title, *keyEq;
      title = [NSString stringWithUTF8String: wv->name];
      if (title == nil)
        title = @"< ? >";  /* (get out in the open so we know about it) */

      keyEq = [self parseKeyEquiv: wv->key];
#ifdef NS_IMPL_COCOA
      /* macOS just ignores modifier strings longer than one character */
      if (keyEquivModMask == 0)
	title = [title stringByAppendingFormat: @"\t%@", keyEq];
      keyEq = @"";
#endif

      item = [self addItemWithTitle: (NSString *)title
                             action: @selector (menuDown:)
                      keyEquivalent: keyEq];
      [item setKeyEquivalentModifierMask: keyEquivModMask];

      [item setEnabled: wv->enabled];

      /* Draw radio buttons and tickboxes */
      if (wv->selected && (wv->button_type == BUTTON_TYPE_TOGGLE ||
                           wv->button_type == BUTTON_TYPE_RADIO))
        [item setState: NSOnState];
      else
        [item setState: NSOffState];

      [item setTag: (NSInteger)wv->call_data];
    }

  return item;
}


/* convenience */
-(void)clear
{
  int n;

  for (n = [self numberOfItems]-1; n >= 0; n--)
    {
      NSMenuItem *item = [self itemAtIndex: n];
      NSString *title = [item title];
      if ([ns_app_name isEqualToString: title]
          && ![item isSeparatorItem])
        continue;
      [self removeItemAtIndex: n];
    }
}


- (void)fillWithWidgetValue: (void *)wvptr
{
  [self fillWithWidgetValue: wvptr frame: (struct frame *)nil];
}

- (void)fillWithWidgetValue: (void *)wvptr frame: (struct frame *)f
{
  widget_value *wv = (widget_value *)wvptr;
  NSFont *menuFont = [NSFont menuFontOfSize:0];
  NSDictionary *attributes =
    [NSDictionary dictionaryWithObject:menuFont forKey:NSFontAttributeName];
  NSSize spaceSize = [@" " sizeWithAttributes:attributes];
  CGFloat maxTabStop = 0;

  /* clear existing contents */
  [self setMenuChangedMessagesEnabled: NO];
  [self clear];

  /* align key bindings
     taken from Yamamoto Mitsuharu's AppKit port */
  for (wv = (widget_value *)wvptr; wv != NULL; wv = wv->next)
    if (!name_is_separator (wv->name) && wv->key)
      {
	NSString *itemName =
	  [NSString stringWithUTF8String:wv->name];
	NSSize size = [[itemName stringByAppendingString:@"\t"]
			sizeWithAttributes:attributes];

	if (maxTabStop < size.width)
	  maxTabStop = size.width;
      }

  for (wv = (widget_value *)wvptr; wv != NULL; wv = wv->next)
    {
    if (!name_is_separator (wv->name) && wv->key)
      {
	NSString *itemName =
	  [NSString stringWithUTF8String:wv->name];
	NSSize nameSize = [itemName sizeWithAttributes:attributes];
	int name_len = strlen (wv->name);
	int pad_len = ceil ((maxTabStop - nameSize.width) / spaceSize.width);

	if (pad_len > 0)
	  {
	    Lisp_Object name;
	    name = make_uninit_string (name_len + pad_len);
	    strcpy (SDATA (name), wv->name);
	    memset (SDATA (name) + name_len, ' ', pad_len);
	    //name[name_len+pad_len] = '\0';
	    wv->name = SDATA (name);
	  }
      }

  /* add new contents */

      NSMenuItem *item = [self addItemWithWidgetValue: wv];
      if (wv->contents)
        {
          EmacsMenu *submenu;

          if (f)
            submenu = [[EmacsMenu alloc] initWithTitle: [item title] frame:f];
          else
            submenu = [[EmacsMenu alloc] initWithTitle: [item title]];

          [self setSubmenu: submenu forItem: item];
          [submenu fillWithWidgetValue: wv->contents];
          [submenu release];
          [item setAction: (SEL)nil];
        }
    }

  [self setMenuChangedMessagesEnabled: YES];
#ifdef NS_IMPL_GNUSTEP
  if ([[self window] isVisible])
    [self sizeToFit];
#endif
}


/* adds an empty submenu and returns it */
- (EmacsMenu *)addSubmenuWithTitle: (const char *)title forFrame: (struct frame *)f
{
  if (! title) return nil;
  NSString *titleStr = [NSString stringWithUTF8String: title];
  if (! titleStr) return nil;
  NSMenuItem *item = [self addItemWithTitle: titleStr
                                     action: (SEL)nil /*@selector (menuDown:) */
                              keyEquivalent: @""];
  EmacsMenu *submenu = [[EmacsMenu alloc] initWithTitle: titleStr frame: f];
  [self setSubmenu: submenu forItem: item];
  [submenu release];
  return submenu;
}

/* run a menu in popup mode */
- (Lisp_Object)runMenuAt: (NSPoint)p forFrame: (struct frame *)f
                 keymaps: (bool)keymaps
{
  EmacsView *view = FRAME_NS_VIEW (f);
  NSEvent *e, *event;
  long retVal;

/*   p = [view convertPoint:p fromView: nil]; */
  p.y = NSHeight ([view frame]) - p.y;
  e = [[view window] currentEvent];
   event = [NSEvent mouseEventWithType: NSRightMouseDown
                              location: p
                         modifierFlags: 0
                             timestamp: [e timestamp]
                          windowNumber: [[view window] windowNumber]
                               context: [e context]
                           eventNumber: 0/*[e eventNumber] */
                            clickCount: 1
                              pressure: 0];

  context_menu_value = -1;
  [NSMenu popUpContextMenu: self withEvent: event forView: view];
  retVal = context_menu_value;
  context_menu_value = 0;
  return retVal > 0
      ? find_and_return_menu_selection (f, keymaps, (void *)retVal)
      : Qnil;
}

@end  /* EmacsMenu */



/* ==========================================================================

    Context Menu: implementing functions

   ========================================================================== */

Lisp_Object
ns_menu_show (struct frame *f, int x, int y, int menuflags,
	      Lisp_Object title, const char **error)
{
  EmacsMenu *pmenu;
  NSPoint p;
  Lisp_Object tem;
  ptrdiff_t specpdl_count = SPECPDL_INDEX ();
  widget_value *wv, *first_wv = 0;
  bool keymaps = (menuflags & MENU_KEYMAPS);

  NSTRACE ("ns_menu_show");

  block_input ();

  p.x = x; p.y = y;

  /* now parse stage 2 as in ns_update_menubar */
  wv = make_widget_value ("contextmenu", NULL, true, Qnil);
  wv->button_type = BUTTON_TYPE_NONE;
  first_wv = wv;

#if 0
  /* FIXME: a couple of one-line differences prevent reuse */
  wv = digest_single_submenu (0, menu_items_used, 0);
#else
  {
  widget_value *save_wv = 0, *prev_wv = 0;
  widget_value **submenu_stack
    = alloca (menu_items_used * sizeof *submenu_stack);
/*   Lisp_Object *subprefix_stack
       = alloca (menu_items_used * sizeof *subprefix_stack); */
  int submenu_depth = 0;
  int first_pane = 1;
  int i;

  /* Loop over all panes and items, filling in the tree.  */
  i = 0;
  while (i < menu_items_used)
    {
      if (EQ (AREF (menu_items, i), Qnil))
	{
	  submenu_stack[submenu_depth++] = save_wv;
	  save_wv = prev_wv;
	  prev_wv = 0;
	  first_pane = 1;
	  i++;
	}
      else if (EQ (AREF (menu_items, i), Qlambda))
	{
	  prev_wv = save_wv;
	  save_wv = submenu_stack[--submenu_depth];
	  first_pane = 0;
	  i++;
	}
      else if (EQ (AREF (menu_items, i), Qt)
	       && submenu_depth != 0)
	i += MENU_ITEMS_PANE_LENGTH;
      /* Ignore a nil in the item list.
	 It's meaningful only for dialog boxes.  */
      else if (EQ (AREF (menu_items, i), Qquote))
	i += 1;
      else if (EQ (AREF (menu_items, i), Qt))
	{
	  /* Create a new pane.  */
	  Lisp_Object pane_name, prefix;
	  const char *pane_string;

	  pane_name = AREF (menu_items, i + MENU_ITEMS_PANE_NAME);
	  prefix = AREF (menu_items, i + MENU_ITEMS_PANE_PREFIX);

#ifndef HAVE_MULTILINGUAL_MENU
	  if (STRINGP (pane_name) && STRING_MULTIBYTE (pane_name))
	    {
	      pane_name = ENCODE_MENU_STRING (pane_name);
	      ASET (menu_items, i + MENU_ITEMS_PANE_NAME, pane_name);
	    }
#endif
	  pane_string = (NILP (pane_name)
			 ? "" : SSDATA (pane_name));
	  /* If there is just one top-level pane, put all its items directly
	     under the top-level menu.  */
	  if (menu_items_n_panes == 1)
	    pane_string = "";

	  /* If the pane has a meaningful name,
	     make the pane a top-level menu item
	     with its items as a submenu beneath it.  */
	  if (!keymaps && strcmp (pane_string, ""))
	    {
	      wv = make_widget_value (pane_string, NULL, true, Qnil);
	      if (save_wv)
		save_wv->next = wv;
	      else
		first_wv->contents = wv;
	      if (keymaps && !NILP (prefix))
		wv->name++;
	      wv->button_type = BUTTON_TYPE_NONE;
	      save_wv = wv;
	      prev_wv = 0;
	    }
	  else if (first_pane)
	    {
	      save_wv = wv;
	      prev_wv = 0;
	    }
	  first_pane = 0;
	  i += MENU_ITEMS_PANE_LENGTH;
	}
      else
	{
	  /* Create a new item within current pane.  */
	  Lisp_Object item_name, enable, descrip, def, type, selected, help;
	  item_name = AREF (menu_items, i + MENU_ITEMS_ITEM_NAME);
	  enable = AREF (menu_items, i + MENU_ITEMS_ITEM_ENABLE);
	  descrip = AREF (menu_items, i + MENU_ITEMS_ITEM_EQUIV_KEY);
	  def = AREF (menu_items, i + MENU_ITEMS_ITEM_DEFINITION);
	  type = AREF (menu_items, i + MENU_ITEMS_ITEM_TYPE);
	  selected = AREF (menu_items, i + MENU_ITEMS_ITEM_SELECTED);
	  help = AREF (menu_items, i + MENU_ITEMS_ITEM_HELP);

#ifndef HAVE_MULTILINGUAL_MENU
          if (STRINGP (item_name) && STRING_MULTIBYTE (item_name))
	    {
	      item_name = ENCODE_MENU_STRING (item_name);
	      ASET (menu_items, i + MENU_ITEMS_ITEM_NAME, item_name);
	    }

          if (STRINGP (descrip) && STRING_MULTIBYTE (descrip))
	    {
	      descrip = ENCODE_MENU_STRING (descrip);
	      ASET (menu_items, i + MENU_ITEMS_ITEM_EQUIV_KEY, descrip);
	    }
#endif /* not HAVE_MULTILINGUAL_MENU */

	  wv = make_widget_value (SSDATA (item_name), NULL, !NILP (enable),
				  STRINGP (help) ? help : Qnil);
	  if (prev_wv)
	    prev_wv->next = wv;
	  else
	    save_wv->contents = wv;
	  if (!NILP (descrip))
	    wv->key = SSDATA (descrip);
	  /* If this item has a null value,
	     make the call_data null so that it won't display a box
	     when the mouse is on it.  */
	  wv->call_data = !NILP (def) ? aref_addr (menu_items, i) : 0;

	  if (NILP (type))
	    wv->button_type = BUTTON_TYPE_NONE;
	  else if (EQ (type, QCtoggle))
	    wv->button_type = BUTTON_TYPE_TOGGLE;
	  else if (EQ (type, QCradio))
	    wv->button_type = BUTTON_TYPE_RADIO;
	  else
	    emacs_abort ();

	  wv->selected = !NILP (selected);

	  prev_wv = wv;

	  i += MENU_ITEMS_ITEM_LENGTH;
	}
    }
  }
#endif

  if (!NILP (title))
    {
      widget_value *wv_title;
      widget_value *wv_sep = make_widget_value ("--", NULL, false, Qnil);

      /* Maybe replace this separator with a bitmap or owner-draw item
	 so that it looks better.  Having two separators looks odd.  */
      wv_sep->next = first_wv->contents;

#ifndef HAVE_MULTILINGUAL_MENU
      if (STRING_MULTIBYTE (title))
	title = ENCODE_MENU_STRING (title);
#endif
      wv_title = make_widget_value (SSDATA (title), NULL, false, Qnil);
      wv_title->button_type = BUTTON_TYPE_NONE;
      wv_title->next = wv_sep;
      first_wv->contents = wv_title;

  pmenu = [[EmacsMenu alloc] initWithTitle:
                               [NSString stringWithUTF8String: SSDATA (title)]];
    }
  else
    {
      pmenu = [[EmacsMenu alloc] initWithTitle: @"Menu"];
    }
  [pmenu fillWithWidgetValue: first_wv->contents];
  free_menubar_widget_value_tree (first_wv);
  unbind_to (specpdl_count, Qnil);

  popup_activated_flag = 1;
  tem = [pmenu runMenuAt: p forFrame: f keymaps: keymaps];
  popup_activated_flag = 0;
  [[FRAME_NS_VIEW (SELECTED_FRAME ()) window] makeKeyWindow];

  unblock_input ();
  return tem;
}


/* ==========================================================================

    Toolbar: externally-called functions

   ========================================================================== */

void
free_frame_tool_bar (struct frame *f)
/* --------------------------------------------------------------------------
    Under NS we just hide the toolbar until it might be needed again.
   -------------------------------------------------------------------------- */
{
  EmacsView *view = FRAME_NS_VIEW (f);

  NSTRACE ("free_frame_tool_bar");

  block_input ();
  view->wait_for_tool_bar = NO;

  FRAME_TOOLBAR_HEIGHT (f) = 0;

  /* Note: This trigger an animation, which calls windowDidResize
     repeatedly. */
  f->output_data.ns->in_animation = 1;
  [[view toolbar] setVisible: NO];
  f->output_data.ns->in_animation = 0;

  unblock_input ();
}

void
update_frame_tool_bar (struct frame *f)
/* --------------------------------------------------------------------------
    Update toolbar contents
   -------------------------------------------------------------------------- */
{
  int i, k = 0;
  EmacsView *view = FRAME_NS_VIEW (f);
  NSWindow *window = [view window];
  EmacsToolbar *toolbar = [view toolbar];
  int oldh;

  NSTRACE ("update_frame_tool_bar");

  if (view == nil || toolbar == nil) return;
  block_input ();

  oldh = FRAME_TOOLBAR_HEIGHT (f);

#ifdef NS_IMPL_COCOA
  [toolbar clearActive];
#else
  [toolbar clearAll];
#endif

  [toolbar setAllowsUserCustomization:YES];
  [toolbar setAutosavesConfiguration:NO];
  /* setAutosavesConfiguration is problematic,
     as it creates a defaults file
     also, doesn't seem to work with our tool bar
     construction mechanism.
     [toolbar setAutosavesConfiguration:YES];

     how to store tool bar changes:

     tool bars here have been set from the tool bar map so that
     there is little access to the original (lisp) item.
     we could create events and process them on the lisp side
     (which will be tricky to get right).

     we need to store
     - enabled items
     - disabled items
     - additional items inserted somewhere

     Lisp event: NS_TOOL_BAR_CONFIG_CHANGED
     (ns-tool-bar-read-configuration)
     => list of strings (containing indexes for current tool-bar-map)

     On the Lisp side, we could then update the tool-bar-map accordingly,
     i.e. reorder it and set visibility flags.
     The internal rep. of the toolbar should be updated soon afterwards
     (perhaps redraw-frame).

     To make the user-mandated changes persistent, we could keep this around:

     ((hash1 . tool-bar-config1)
      (hash2 . tool-bar-config2))

      The hashes are hashes of the set of Lisp-side toolbar menu items (via their event names)
      (sxhash (sort (mapcar
      (lambda (m)
      (when (consp m)
      (car m)))
      tool-bar-map)))

      however, as soon as an item is added to or removed from the tool bar, do we want to
      discard the user's tool bar configuration?
      Maybe that's okay.

      the above tool-bar-config1 would be a list of toolbar item identifiers,
      indicating visibility (by presence) and ordering.
      It could be used to update the toolbar when desired.

  */


  /* update EmacsToolbar as in GtkUtils, build items list */
  for (i = 0; i < f->n_tool_bar_items; ++i)
    {
#define TOOLPROP(IDX) AREF (f->tool_bar_items, \
                            i * TOOL_BAR_ITEM_NSLOTS + (IDX))

      BOOL enabled_p = !NILP (TOOLPROP (TOOL_BAR_ITEM_ENABLED_P));
      BOOL visible_p = !NILP (TOOLPROP (TOOL_BAR_ITEM_VISIBLE_P));
      BOOL selected_p = !NILP (TOOLPROP (TOOL_BAR_ITEM_SELECTED_P));
      int idx;
      ptrdiff_t img_id;
      struct image *img;
      Lisp_Object image;
      Lisp_Object helpObj;
      Lisp_Object captionObj;
      char *captionText;
      char *keyText;
      const char *helpText;
      Lisp_Object label = TOOLPROP (TOOL_BAR_ITEM_LABEL);
      Lisp_Object key = TOOLPROP (TOOL_BAR_ITEM_KEY);

      if (STRINGP (key))
      	keyText = (char *) SSDATA (key);
      else if (SYMBOLP (key))
      	keyText = (char *) SSDATA (SYMBOL_NAME (key) );
      else
      	keyText = "?";

      /* Check if this is a separator.  */
      if (// EQ (TOOLPROP (TOOL_BAR_ITEM_TYPE), Qt) ||
	  (STRINGP (label) && strcmp("--", SSDATA (label)) == 0))
        {
          [toolbar addDisplayItemSpacerWithIdx: k++ tag:i key: keyText];
          continue;
        }

      /* If image is a vector, choose the image according to the
	 button state.  */
      image = TOOLPROP (TOOL_BAR_ITEM_IMAGES);
      if (VECTORP (image))
	{
          /* NS toolbar auto-computes disabled and selected images */
          idx = TOOL_BAR_IMAGE_ENABLED_SELECTED;
	  eassert (ASIZE (image) >= idx);
	  image = AREF (image, idx);
	}
      else
        {
          idx = -1;
        }
      helpObj = TOOLPROP (TOOL_BAR_ITEM_HELP);
      if (NILP (helpObj))
        helpObj = TOOLPROP (TOOL_BAR_ITEM_CAPTION);
      helpText = STRINGP (helpObj) ? SSDATA (helpObj) : "";

      /* Ignore invalid image specifications.  */
      if (!valid_image_p (image))
        {
          /* Don't log anything, GNUS makes invalid images all the time.  */
          continue;
        }

      img_id = lookup_image (f, image);
      img = IMAGE_FROM_ID (f, img_id);
      if (img == nil)
        {
          NSLog (@"Could not prepare toolbar image for display (1).");
          continue;
        }
      prepare_image_for_display (f, img);

      if (img->load_failed_p || img->pixmap == nil)
        {
          NSLog (@"Could not prepare toolbar image for display (2).");
          continue;
        }
      captionObj = TOOLPROP (TOOL_BAR_ITEM_LABEL);
      captionText = STRINGP (captionObj) ? SSDATA (captionObj) : "";

      [toolbar addDisplayItemWithImage: img->pixmap
                                   idx: k++
                                   tag: i
                              helpText: helpText
                               enabled: enabled_p
			       visible: visible_p
				   key: keyText
			     labelText: captionText];

#undef TOOLPROP
    }

  if (![toolbar isVisible])
    {
      f->output_data.ns->in_animation = 1;
      [toolbar setVisible: YES];
      f->output_data.ns->in_animation = 0;
    }

#ifdef NS_IMPL_COCOA
  if ([toolbar changed])
    {
      /* inform app that toolbar has changed */
      NSDictionary *dict = [toolbar configurationDictionary];
      NSMutableDictionary *newDict = [dict mutableCopy];
      NSEnumerator *keys = [[dict allKeys] objectEnumerator];
      id key;
      while ((key = [keys nextObject]) != nil)
        {
          NSObject *val = [dict objectForKey: key];
          if ([val isKindOfClass: [NSArray class]])
            {
              [newDict setObject:
                         [toolbar toolbarDefaultItemIdentifiers: toolbar]
                          forKey: key];
              break;
            }
        }
      [toolbar setConfigurationFromDictionary: newDict];
      [newDict release];
    }
#endif

  FRAME_TOOLBAR_HEIGHT (f) =
    NSHeight ([window frameRectForContentRect: NSMakeRect (0, 0, 0, 0)])
    - FRAME_NS_TITLEBAR_HEIGHT (f);
  if (FRAME_TOOLBAR_HEIGHT (f) < 0) // happens if frame is fullscreen.
    FRAME_TOOLBAR_HEIGHT (f) = 0;

  if (oldh != FRAME_TOOLBAR_HEIGHT (f))
    [view updateFrameSize:YES];
  if (view->wait_for_tool_bar && FRAME_TOOLBAR_HEIGHT (f) > 0)
    {
      view->wait_for_tool_bar = NO;
      [view setNeedsDisplay: YES];
    }

  unblock_input ();
}


DEFUN ("ns-tool-bar-customize", Fns_tool_bar_customize, Sns_tool_bar_customize, 0, 1, "",
       doc: /* View tool bar configuration.
Shows the tool bar customization panel in the given frame.
The tool bar should be visible in FRAME when calling this function.*/)
     (frame)
     Lisp_Object frame;
{

  struct frame *f = nil;

  if (NILP (frame) )
    f = SELECTED_FRAME ();
  else
    {
      CHECK_FRAME (frame);
      f = XFRAME (frame);
    }

  block_input();
  update_frame_tool_bar (f);  /* fill in items */
  [[FRAME_NS_VIEW (f) toolbar] setVisible: YES];
  [[FRAME_NS_VIEW (f) toolbar] runCustomizationPalette:FRAME_NS_VIEW (f)];
  unblock_input();
  return Qnil;
}

DEFUN ("ns-tool-bar-configuration", Fns_tool_bar_configuration, Sns_tool_bar_configuration, 0, 1, 0,
       doc: /* Return tool bar configuration.
Evaluates to a list of menu item keys corresponding
to elements of the tool bar map active in frame FRAME.
The presence of an item in this list indicates visibility,
the order indicates order in the tool bar, both as
set by the user.
Items in this list are always Lisp symbols.*/)
     (frame)
     Lisp_Object frame;
{

  struct frame *f = nil;

  if (NILP (frame) )
    f = SELECTED_FRAME ();
  else
    {
      CHECK_FRAME (frame);
      f = XFRAME (frame);
    }

  block_input();
  Lisp_Object item_identifiers = Qnil;
  NSEnumerator *itemEnum = [[[FRAME_NS_VIEW (f) toolbar] items] reverseObjectEnumerator];
  NSToolbarItem *item = nil;
  while (item = [itemEnum nextObject])
    item_identifiers = Fcons (([item itemIdentifier] == NSToolbarFlexibleSpaceItemIdentifier)
			      ? Qnil :
			      /* Lisp symbol extracted from identifier string.
				 ID string contains image hash as well.*/
			      intern ([([[item itemIdentifier] hasPrefix: @"0x"] == YES ?
					[[item itemIdentifier] substringFromIndex:10] :
					[item itemIdentifier])
					UTF8String]),
			      item_identifiers);
  unblock_input();
  return item_identifiers;
}


/* ==========================================================================

    Toolbar: class implementation

   ========================================================================== */

@implementation EmacsToolbar

#define NSTOOLBAR_NEEDED_DISPLAY_MODE (EQ (Vns_tool_bar_display_mode, intern ("labels")) ? \
				       NSToolbarDisplayModeLabelOnly : \
				       ( EQ (Vns_tool_bar_display_mode, intern ("both")) ? \
					 NSToolbarDisplayModeIconAndLabel : \
					 ( EQ (Vns_tool_bar_display_mode, intern ("icons")) ? \
					   NSToolbarDisplayModeIconOnly : \
					   NSToolbarDisplayModeDefault)))

#define NSTOOLBAR_NEEDED_SIZE_MODE (EQ (Vns_tool_bar_size_mode, intern ("small")) ? \
				    NSToolbarSizeModeSmall :		\
				    ( EQ (Vns_tool_bar_size_mode, intern ("regular")) ? \
				      NSToolbarSizeModeRegular :	\
				      NSToolbarSizeModeDefault))

- initForView: (EmacsView *)view withIdentifier: (NSString *)identifier
{
  NSTRACE ("[EmacsToolbar initForView: withIdentifier:]");

  self = [super initWithIdentifier: identifier];
  emacsView = view;
  [super setSizeMode: NSTOOLBAR_NEEDED_SIZE_MODE];
  [super setDisplayMode: NSTOOLBAR_NEEDED_DISPLAY_MODE];
  [self setDelegate: self];
  identifierToItem = [[NSMutableDictionary alloc] initWithCapacity: 10];
  activeIdentifiers = [[NSMutableArray alloc] initWithCapacity: 8];
  availableIdentifiers = [[NSMutableArray alloc] initWithCapacity: 50];
  prevIdentifiers = nil;
  prevEnablement = enablement = 0L;
  return self;
}

- (void)dealloc
{
  NSTRACE ("[EmacsToolbar dealloc]");

  [prevIdentifiers release];
  [activeIdentifiers release];
  [availableIdentifiers release];
  [identifierToItem release];
  [super dealloc];
}

- (void) clearActive
{
  NSTRACE ("[EmacsToolbar clearActive]");

  [prevIdentifiers release];
  prevIdentifiers = [activeIdentifiers copy];
  [activeIdentifiers removeAllObjects];
  /* will be filled from invisible objects of current toolbar */
  [availableIdentifiers removeAllObjects];
  prevEnablement = enablement;
  enablement = 0L;
  [self setSizeMode: NSTOOLBAR_NEEDED_SIZE_MODE];
  [self setDisplayMode: NSTOOLBAR_NEEDED_DISPLAY_MODE];
}

- (void) clearAll
{
  NSTRACE ("[EmacsToolbar clearAll]");

  [self clearActive];
  while ([[self items] count] > 0)
    [self removeItemAtIndex: 0];
}

- (void)setDisplayMode:(NSToolbarDisplayMode)displayMode
{
  [super setDisplayMode:displayMode];

  if ([self displayMode] == NSToolbarDisplayModeDefault)
    Vns_tool_bar_display_mode = Qnil;
  else if ([self displayMode] == NSToolbarDisplayModeIconOnly)
    Vns_tool_bar_display_mode = intern ("icons");
  else if ([self displayMode] == NSToolbarDisplayModeIconAndLabel)
    Vns_tool_bar_display_mode = intern ("both");
  else if ([self displayMode] == NSToolbarDisplayModeLabelOnly)
    Vns_tool_bar_display_mode = intern ("labels");
}

- (void)setSizeMode:(NSToolbarSizeMode)sizeMode
{
  [super setSizeMode:sizeMode];

  if ([self sizeMode] == NSToolbarSizeModeDefault)
    Vns_tool_bar_size_mode = Qnil;
  else if ([self sizeMode] == NSToolbarSizeModeRegular)
    Vns_tool_bar_size_mode = intern ("regular");
  else if ([self sizeMode] == NSToolbarSizeModeSmall)
    Vns_tool_bar_size_mode = intern ("small");

}

- (BOOL) changed
{
  NSTRACE ("[EmacsToolbar changed]");

  return [activeIdentifiers isEqualToArray: prevIdentifiers] &&
    enablement == prevEnablement ? NO : YES;
}

- (void) addDisplayItemSpacerWithIdx: (int)idx
				 tag: (int)tag
				 key: (char *) key
{
  /* 1) come up w/identifier */
  NSString *identifier = NSToolbarFlexibleSpaceItemIdentifier;
  // [NSString stringWithCString: key];

  /* 2) create / reuse item */
  NSToolbarItem *item = [identifierToItem objectForKey: identifier];
  if (item == nil)
    {
      item = [[[NSToolbarItem alloc] initWithItemIdentifier:
	       NSToolbarFlexibleSpaceItemIdentifier]
               autorelease];
    }
  [item setTag: tag];
  [item setLabel: @"--"];

  /* 3) update state */
  [identifierToItem setObject: item forKey: identifier];
  [availableIdentifiers addObject: identifier];
  [activeIdentifiers addObject: identifier];

  enablement = (enablement << 1) | false;
}

- (void) addDisplayItemWithImage: (EmacsImage *)img
                             idx: (int)idx
                             tag: (int)tag
                        helpText: (const char *)help
                         enabled: (BOOL)enabled
		      visible: (BOOL)visible
		          key: (char *)key
		      labelText: (char *)label
{
  NSTRACE ("[EmacsToolbar addDisplayItemWithImage: ...]");



  NSString *label_str = @"";
  NSString *help_str = @"";
  if (label != 0)
    label_str = [NSString stringWithCString: label encoding:NSASCIIStringEncoding];
  if (help != 0)
    help_str = [NSString stringWithCString: help encoding:NSASCIIStringEncoding];

  /* 1) come up w/identifier */
  // NSString *identifier
  //   = [NSString stringWithFormat: @"%lu", (unsigned int)[img hash]];
  NSString *identifier = [NSString stringWithFormat: @"0x%08lX%s",
				   ([img hash] + [label_str hash] + [help_str hash]) & 0xFFFFFFFF, key];

  /* 2) create / reuse item */
  NSToolbarItem *item = [identifierToItem objectForKey: identifier];
  if (item == nil)
    {
      item = [[[NSToolbarItem alloc] initWithItemIdentifier: identifier]
               autorelease];
      [item setImage: img]; // image is a strong property, will retain as needed.
      [item setToolTip: help_str];
      [item setLabel: label_str];
      [item setPaletteLabel: label_str];
      [item setTarget: emacsView];
      [item setAction: @selector (toolbarClicked:)];
      [identifierToItem setObject: item forKey: identifier];
    }

#ifdef NS_IMPL_GNUSTEP
  [self insertItemWithItemIdentifier: identifier atIndex: idx];
#endif

  [item setTag: tag];
  [item setEnabled: enabled];

  /* 3) update state */
  [identifierToItem setObject: item forKey: identifier];
  [availableIdentifiers addObject: identifier];
  if (visible)
    [activeIdentifiers addObject: identifier];
  enablement = (enablement << 1) | (enabled == YES);
}

/* This overrides super's implementation, which automatically sets
   all items to enabled state (for some reason). */
- (void)validateVisibleItems
{
  NSTRACE ("[EmacsToolbar validateVisibleItems]");
}


/* delegate methods */

- (NSToolbarItem *)toolbar: (NSToolbar *)toolbar
      itemForItemIdentifier: (NSString *)itemIdentifier
  willBeInsertedIntoToolbar: (BOOL)flag
{
  NSTRACE ("[EmacsToolbar toolbar: ...]");

  /* look up NSToolbarItem by identifier and return... */
  return [identifierToItem objectForKey: itemIdentifier];
}

- (NSArray *)toolbarDefaultItemIdentifiers: (NSToolbar *)toolbar
{
  NSTRACE ("[EmacsToolbar toolbarDefaultItemIdentifiers:]");

  /* return entire set.. */
  return activeIdentifiers;
}

/* for configuration palette */
- (NSArray *)toolbarAllowedItemIdentifiers: (NSToolbar *)toolbar
{
  NSTRACE ("[EmacsToolbar toolbarAllowedItemIdentifiers:]");

  /* return entire set... */
  return availableIdentifiers; // [identifierToItem allKeys];
}
- (void)toolbarDidRemoveItem:(NSNotification *)notification
{
  //  if ([self customizationPaletteIsRunning])
    [self customizationDidChange];
}

- (void)customizationDidChange
{
  // BLOCK_INPUT;
  // Lisp_Object frame = Qnil;
  // XSETFRAME (frame, emacsView->emacsframe);
  // Lisp_Object args[2];
  // args[0] = Qns_tool_bar_customized_hook;
  // args[1] = frame;
  // disableHooks = YES;
  // Frun_hook_with_args (2, args);
  // disableHooks = NO;
  // UNBLOCK_INPUT;

  // send event
  [emacsView toolbarCustomized: self];
}

// /* toolbarWillAddItem is called before the item is added,
//    so it's useless for the purposes of running the hook. */
// hack
- (void)checkCustomizationChange:(NSTimer*)theTimer
{
  if (! [self customizationPaletteIsRunning])
    { [theTimer invalidate];
      [self customizationDidChange];
    }
}
- (void)runCustomizationPalette:(id)sender
{
  [super runCustomizationPalette:sender];
  [NSTimer scheduledTimerWithTimeInterval: (float)0.1 target: self
  				 selector: @selector (checkCustomizationChange:)
  				 userInfo: nil repeats: YES];
}
// not called upon changing tool bar
// - (void)validateVisibleItems:(id)sender

/* it is currently impossible to have all toolbar modifications trigger
send a message; Command-Option dragging items for instance does not trigger
a notification */

- (void)setVisible:(BOOL)shown
{
  NSTRACE ("[EmacsToolbar setVisible:%d]", shown);

  [super setVisible:shown];
}


/* optional and unneeded */
// - (void)insertItemWithItemIdentifier:(NSString *)itemIdentifier atIndex:(NSInteger)index
// - (void)setConfigurationFromDictionary:(NSDictionary *)configDict
/* - (NSArray *)toolbarSelectableItemIdentifiers: (NSToolbar *)toolbar */

@end  /* EmacsToolbar */



/* ==========================================================================

    Tooltip: class implementation

   ========================================================================== */

/* Needed because NeXTstep does not provide enough control over tooltip
   display. */
@implementation EmacsTooltip

- init
{
  NSColor *col = [NSColor colorWithCalibratedRed: 1.0 green: 1.0
                                            blue: 0.792 alpha: 0.95];
  NSFont *font = [NSFont toolTipsFontOfSize: 0];
  NSFont *sfont = [font screenFont];
  int height = [sfont ascender] - [sfont descender];
/*[font boundingRectForFont].size.height; */
  NSRect r = NSMakeRect (0, 0, 100, height+6);

  textField = [[NSTextField alloc] initWithFrame: r];
  [textField setFont: font];
  [textField setBackgroundColor: col];

  [textField setEditable: NO];
  [textField setSelectable: NO];
  [textField setBordered: NO];
  [textField setBezeled: NO];
  [textField setDrawsBackground: YES];

  win = [[NSWindow alloc]
            initWithContentRect: [textField frame]
                      styleMask: 0
                        backing: NSBackingStoreBuffered
                          defer: NO];
  [win setDelegate: self];
  [win setReleasedWhenClosed:NO];  // we release it explicitly with the enclosing EmacsToolTip objecct
  [[win contentView] addSubview: textField];
/*  [win setBackgroundColor: col]; */
  [win setOpaque: NO];

  if ([win respondsToSelector:@selector(setAnimationBehavior:)])
    [win setAnimationBehavior:NSWindowAnimationBehaviorNone];
  [win setAutodisplay:NO];
  [win setHasShadow:YES];
  [win setLevel:NSScreenSaverWindowLevel];

  return self;
}

- (void) dealloc
{
  [win close];
  [win release];
  [textField release];
  [super dealloc];
}

- (void) setText: (char *)text
{
  NSString *str = [NSString stringWithUTF8String: text];
  NSRect r  = [textField frame];
  NSSize tooltipDims;

  [textField setStringValue: str];
  tooltipDims = [[textField cell] cellSize];

  r.size.width = tooltipDims.width;
  r.size.height = tooltipDims.height;
  [textField setFrame: r];
}

- (void) showAtX: (int)x Y: (int)y for: (int)seconds
{
  if (win == nil)  // window not (yet) available - don't show the tooltip (for safety)
    return;

  NSRect wr = [win frame];

  wr.origin = NSMakePoint (x, y);
  wr.size = [textField frame].size;

  [win setFrame: wr display: YES];
  [win setLevel: NSPopUpMenuWindowLevel];
  [win orderFront: self];
  [win display];
  timer = [NSTimer scheduledTimerWithTimeInterval: (float)seconds target: self
                                         selector: @selector (hide)
                                         userInfo: nil repeats: NO];
  [timer retain];
}

- (void) hide
{
  [win close];  // set to not release here
  if (timer != nil)
    {
      if ([timer isValid])
        [timer invalidate];
      [timer release];
      timer = nil;
    }
}

- (BOOL) isActive
{
  return timer != nil;
}

- (NSRect) frame
{
  return [textField frame];
}

@end  /* EmacsTooltip */



/* EmacsALertPanel  (leftover?)*/


@implementation EmacsAlertPanel
- init
{
  [super init];
  returnValues = calloc(sizeof(Lisp_Object), 20);
  returnValueCount = 0;
  return self;
}
- (void)dealloc
{
  free(returnValues);
  [super dealloc];
}

- (void)close
{
   [[self window] close];
    }

- (void) alertDidEnd:(NSAlert *)alert returnCode:(NSInteger)returnCode contextInfo:(void *)contextInfo
    {
  * ((NSInteger*) contextInfo) = returnCode;
        }

/* do to: move this into init: */
- (void) processDialogFromList: (Lisp_Object)list
{
  Lisp_Object item;
  int cancel = 1;
  for (; CONSP (list) && returnValueCount<20; list = XCDR (list))
    {
      item = XCAR (list);

      if (STRINGP (item))
        { /* inactive button */
          [[self addButtonWithTitle: [NSString stringWithUTF8String: SDATA (item)] ] setEnabled:NO];
    }
      else if (NILP (item))
        { /* unfortunately, NSAlert will resize this button.  We can
	     only customize after a call to update:, but then the
	     other buttons are already positioned so it would be
	     pointless.  */
	  NSButton *space = [self addButtonWithTitle: @" " ];
	  [space setHidden:YES];
}
      else if (CONSP (item))
{
	  NSString *title = @"malformed";
	  NSString *key = nil;
	  NSButton *button = nil;
	  if (CONSP (XCAR (item))) /* key specified? */
{
	      if (STRINGP (XCAR (XCAR (item))))
		title = [NSString stringWithUTF8String: SDATA (XCAR (XCAR (item)))];
	      if (INTEGERP ( XCDR (XCAR (item))))
		key =  [[NSString stringWithFormat: @"%c", (int) XINT (XCDR (XCAR (item)))] retain];
	      else
		key = nil;
}
  else
  {
	      if (STRINGP (XCAR (item)))
		title = [NSString stringWithUTF8String: SDATA (XCAR (item))];
          }
	  if (EQ (XCDR (item), intern ("suppress")))
    {
	      [self setShowsSuppressionButton:YES];
	      button = [self suppressionButton];
	      [button setTitle:title];
        }
	  else
	    { /* normal button*/
	      button = [self addButtonWithTitle: title];
    }
	  [button setTag: returnValueCount];
	  if (key)
      {
	      [button setKeyEquivalent: key];
	      /* buttons like Don't Save have a non-nil modifier
		 by default.  We have to reset that. */
	      [button setKeyEquivalentModifierMask: 0];
      }
	  returnValues[returnValueCount++] = XCDR (item);
      }
      else if (EQ (item, intern ("cancel")))
	{ /* add cancel button */
	  [[self addButtonWithTitle:  @"Cancel"] setTag: -2];
	  cancel = 0;
  }
      else if (EQ (item, intern ("no-cancel")))
	{ /* skip cancel button */
	  cancel = 0;
}
}

  if (cancel || returnValueCount == 0)
    [[self addButtonWithTitle: @"Cancel"] setTag: -2];
        }

@end


/* ==========================================================================

    Popup Dialog: implementing functions

   ========================================================================== */

struct Popdown_data
{
  EmacsAlertPanel *dialog;
};

static void
pop_down_menu (void *arg)
{
  struct Popdown_data *unwind_data = arg;

  block_input ();
  if (popup_activated_flag)
    {
      EmacsDialogPanel *panel = (EmacsDialogPanel *) unwind_data->dialog;
      popup_activated_flag = 0;
      [NSApp endModalSession: popupSession];
      if (popupSheetAlert)
	{
	  [NSApp endSheet:[popupSheetAlert window]];
	  [popupSheetAlert release];
	} else
	{
      [panel close];
	}

      [[FRAME_NS_VIEW (SELECTED_FRAME ()) window] makeKeyWindow];
    }

  xfree (unwind_data);
  unblock_input ();
}

// extern EMACS_TIME timer_check ();
extern struct timespec timer_check (void);

Lisp_Object
ns_popup_dialog (struct frame *f, Lisp_Object header, Lisp_Object contents)
{

  // extern EMACS_TIME timer_check (void);
  // EmacsAlertPanel *dialog;
  Lisp_Object window;
  BOOL useSheet = YES;

  id dialog;
  Lisp_Object tem, title;
  BOOL isQ;

  NSTRACE ("ns_popup_dialog");

  check_window_system (NULL);

  CHECK_CONS (contents);
  isQ = NILP (header);


  if (f==NULL)
    {
      useSheet = NO;
    }

  if (f)
    {
  check_window_system (f);

    }

  title = Fcar (contents);
  CHECK_STRING (title);

  if (NILP (Fcar (Fcdr (contents))))
    /* No buttons specified, add an "Ok" button so users can pop down
       the dialog.  */
    contents = list2 (title, Fcons (build_string ("Ok"), Qt));

  block_input ();

  dialog = [[EmacsAlertPanel alloc] init];

  if (NILP (header))
    [dialog setAlertStyle: NSWarningAlertStyle];
  else if (EQ (header, intern ("critical")))
    [dialog setAlertStyle: NSCriticalAlertStyle];
  else
    [dialog setAlertStyle: NSInformationalAlertStyle];


  Lisp_Object head;
  /* read contents */
  if (XTYPE (contents) == Lisp_Cons)
  {
      head = Fcar (contents);
      [((EmacsAlertPanel*)dialog) processDialogFromList: Fcdr (contents)];
}
  else
    head = contents;

  if (XTYPE (head) == Lisp_String)
{
      char* split = strchr( SDATA (head), '\n');
      if ( split )
{
	  split[0] = '\0';
	  [dialog setMessageText:
		   [NSString stringWithUTF8String: SDATA (head)]];
	  split[0] = '\n';  /* not nice, but works */
	  [dialog setInformativeText:
		   [NSString stringWithUTF8String: split+1]];
	} else
    {
	  [dialog setMessageText:
		   [NSString stringWithUTF8String: SDATA (head)]];
    }
    }

  {
    int i;

  NSInteger ret = -1;

  {
    ptrdiff_t specpdl_count = SPECPDL_INDEX ();
    struct Popdown_data *unwind_data = xmalloc (sizeof (*unwind_data));

    popup_activated_flag = 1;

    unwind_data->dialog = dialog;

    record_unwind_protect_ptr (pop_down_menu, unwind_data);


  [dialog layout]; /* because we may not call beginSheet / runModal */

  if (useSheet)
{
      [dialog beginSheetModalForWindow:[FRAME_NS_VIEW (f) window]
			 modalDelegate:dialog
			didEndSelector:@selector(alertDidEnd:returnCode:contextInfo:)
			 contextInfo:&ret];
      popupSheetAlert = dialog; /* store so the sheet will be ended. */
}
  else
    popupSheetAlert = nil;


  /* initiate a session that will be ended by pop_down_menu */
  [dialog retain];
  popupSession = [NSApp beginModalSessionForWindow: [dialog window]];

  int ret2 = -1;
  while (popup_activated_flag
	 && ret == -1
         && ((ret2 = [NSApp runModalSession: popupSession])
	     == NSRunContinuesResponse))
    {
      /* Run this for timers.el, indep of atimers; might not return.
         TODO: use return value to avoid calling every iteration. */
      timer_check ();
      [NSThread sleepUntilDate: [NSDate dateWithTimeIntervalSinceNow: 0.1]];
        }
  if (ret == -1 && ret2 != -1)
    ret = ret2;

  if (ret>=0 && ret < ((EmacsAlertPanel *) dialog)->returnValueCount)
        {
      // *(EMACS_INT*)(&tem)
	  tem = (Lisp_Object) ((EmacsAlertPanel *) dialog)->returnValues[ret];
      if ([[dialog suppressionButton] state] == NSOnState)
        {
	  tem = Fcons (tem, ((EmacsAlertPanel *) dialog)->returnValues[[[dialog suppressionButton] tag]]);
        }
    }
    unbind_to (specpdl_count, Qnil);  /* calls pop_down_menu */
  }
  unblock_input ();
  [dialog release];
  if (ret==-2) /*cancel*/
     Fsignal (Qquit, Qnil); /*special button value for cancel*/

  return tem;

}
}

/* ==========================================================================

    Popup Dialog: class implementation

   ========================================================================== */

@interface FlippedView : NSView
{
}
@end

@implementation FlippedView
- (BOOL)isFlipped
{
  return YES;
}
@end

@implementation EmacsDialogPanel

#define SPACER		8.0
#define ICONSIZE	64.0
#define TEXTHEIGHT	20.0
#define MINCELLWIDTH	90.0

- initWithContentRect: (NSRect)contentRect styleMask: (NSUInteger)aStyle
              backing: (NSBackingStoreType)backingType defer: (BOOL)flag
{
  NSSize spacing = {SPACER, SPACER};
  NSRect area;
  id cell;
  NSImageView *imgView;
  FlippedView *contentView;
  NSImage *img;

  dialog_return   = Qundefined;
  button_values   = NULL;
  area.origin.x   = 3*SPACER;
  area.origin.y   = 2*SPACER;
  area.size.width = ICONSIZE;
  area.size.height= ICONSIZE;
  img = [[NSImage imageNamed: @"NSApplicationIcon"] copy];
#ifdef NS_IMPL_COCOA
#if MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_6
  [img setScalesWhenResized: YES];
#endif
#endif
  [img setSize: NSMakeSize (ICONSIZE, ICONSIZE)];
  imgView = [[NSImageView alloc] initWithFrame: area];
  [imgView setImage: img];
  [imgView setEditable: NO];
  [img autorelease];
  [imgView autorelease];

  aStyle = NSTitledWindowMask|NSClosableWindowMask|NSUtilityWindowMask;
  flag = YES;
  rows = 0;
  cols = 1;
  [super initWithContentRect: contentRect styleMask: aStyle
                     backing: backingType defer: flag];
  contentView = [[FlippedView alloc] initWithFrame: [[self contentView] frame]];
  [contentView autorelease];

  [self setContentView: contentView];

  [[self contentView] setAutoresizesSubviews: YES];

  [[self contentView] addSubview: imgView];
  [self setTitle: @""];

  area.origin.x   += ICONSIZE+2*SPACER;
/*  area.origin.y   = TEXTHEIGHT; ICONSIZE/2-10+SPACER; */
  area.size.width = 400;
  area.size.height= TEXTHEIGHT;
  command = [[[NSTextField alloc] initWithFrame: area] autorelease];
  [[self contentView] addSubview: command];
  [command setStringValue: ns_app_name];
  [command setDrawsBackground: NO];
  [command setBezeled: NO];
  [command setSelectable: NO];
  [command setFont: [NSFont boldSystemFontOfSize: 13.0]];

/*  area.origin.x   = ICONSIZE+2*SPACER;
  area.origin.y   = TEXTHEIGHT + 2*SPACER;
  area.size.width = 400;
  area.size.height= 2;
  tem = [[[NSBox alloc] initWithFrame: area] autorelease];
  [[self contentView] addSubview: tem];
  [tem setTitlePosition: NSNoTitle];
  [tem setAutoresizingMask: NSViewWidthSizable];*/

/*  area.origin.x = ICONSIZE+2*SPACER; */
  area.origin.y += TEXTHEIGHT+SPACER;
  area.size.width = 400;
  area.size.height= TEXTHEIGHT;
  title = [[[NSTextField alloc] initWithFrame: area] autorelease];
  [[self contentView] addSubview: title];
  [title setDrawsBackground: NO];
  [title setBezeled: NO];
  [title setSelectable: NO];
  [title setFont: [NSFont systemFontOfSize: 11.0]];

  cell = [[[NSButtonCell alloc] initTextCell: @""] autorelease];
  [cell setBordered: NO];
  [cell setEnabled: NO];
  [cell setCellAttribute: NSCellIsInsetButton to: 8];
  [cell setBezelStyle: NSRoundedBezelStyle];

  matrix = [[NSMatrix alloc] initWithFrame: contentRect
                                      mode: NSHighlightModeMatrix
                                 prototype: cell
                              numberOfRows: 0
                           numberOfColumns: 1];
  [matrix setFrameOrigin: NSMakePoint (area.origin.x,
                                      area.origin.y + (TEXTHEIGHT+3*SPACER))];
  [matrix setIntercellSpacing: spacing];
  [matrix autorelease];

  [[self contentView] addSubview: matrix];
  [self setOneShot: YES];
  [self setReleasedWhenClosed: YES];
  [self setHidesOnDeactivate: YES];
  return self;
}


- (BOOL)windowShouldClose: (id)sender
{
  window_closed = YES;
  [NSApp stop:self];
  return NO;
}

- (void)dealloc
{
  xfree (button_values);
  [super dealloc];
}

- (void)process_dialog: (Lisp_Object) list
{
  Lisp_Object item, lst = list;
  int row = 0;
  int buttons = 0, btnnr = 0;

  for (; XTYPE (lst) == Lisp_Cons; lst = XCDR (lst))
    {
      item = XCAR (list);
      if (XTYPE (item) == Lisp_Cons)
        ++buttons;
    }

  if (buttons > 0)
    button_values = xmalloc (buttons * sizeof *button_values);

  for (; XTYPE (list) == Lisp_Cons; list = XCDR (list))
    {
      item = XCAR (list);
      if (XTYPE (item) == Lisp_String)
        {
          [self addString: SSDATA (item) row: row++];
        }
      else if (XTYPE (item) == Lisp_Cons)
        {
          button_values[btnnr] = XCDR (item);
          [self addButton: SSDATA (XCAR (item)) value: btnnr row: row++];
          ++btnnr;
        }
      else if (NILP (item))
        {
          [self addSplit];
          row = 0;
        }
    }
}


- (void)addButton: (char *)str value: (int)tag row: (int)row
{
  id cell;

  if (row >= rows)
    {
      [matrix addRow];
      rows++;
    }
  cell = [matrix cellAtRow: row column: cols-1];
  [cell setTarget: self];
  [cell setAction: @selector (clicked: )];
  [cell setTitle: [NSString stringWithUTF8String: str]];
  [cell setTag: tag];
  [cell setBordered: YES];
  [cell setEnabled: YES];
}


- (void)addString: (char *)str row: (int)row
{
  id cell;

  if (row >= rows)
    {
      [matrix addRow];
      rows++;
    }
  cell = [matrix cellAtRow: row column: cols-1];
  [cell setTitle: [NSString stringWithUTF8String: str]];
  [cell setBordered: YES];
  [cell setEnabled: NO];
}


- (void)addSplit
{
  [matrix addColumn];
  cols++;
}


- (void)clicked: sender
{
  NSArray *sellist = nil;
  EMACS_INT seltag;

  sellist = [sender selectedCells];
  if ([sellist count] < 1)
    return;

  seltag = [[sellist objectAtIndex: 0] tag];
  dialog_return = button_values[seltag];
  [NSApp stop:self];
}


- initFromContents: (Lisp_Object)contents isQuestion: (BOOL)isQ
{
  Lisp_Object head;
  [super init];

  if (XTYPE (contents) == Lisp_Cons)
    {
      head = Fcar (contents);
      [self process_dialog: Fcdr (contents)];
    }
  else
    head = contents;

  if (XTYPE (head) == Lisp_String)
      [title setStringValue:
                 [NSString stringWithUTF8String: SSDATA (head)]];
  else if (isQ == YES)
      [title setStringValue: @"Question"];
  else
      [title setStringValue: @"Information"];

  {
    int i;
    NSRect r, s, t;

    if (cols == 1 && rows > 1)	/* Never told where to split */
      {
        [matrix addColumn];
        for (i = 0; i < rows/2; i++)
          {
            [matrix putCell: [matrix cellAtRow: (rows+1)/2 column: 0]
                      atRow: i column: 1];
            [matrix removeRow: (rows+1)/2];
          }
      }

    [matrix sizeToFit];
    {
      NSSize csize = [matrix cellSize];
      if (csize.width < MINCELLWIDTH)
        {
          csize.width = MINCELLWIDTH;
          [matrix setCellSize: csize];
          [matrix sizeToCells];
        }
    }

    [title sizeToFit];
    [command sizeToFit];

    t = [matrix frame];
    r = [title frame];
    if (r.size.width+r.origin.x > t.size.width+t.origin.x)
      {
        t.origin.x   = r.origin.x;
        t.size.width = r.size.width;
      }
    r = [command frame];
    if (r.size.width+r.origin.x > t.size.width+t.origin.x)
      {
        t.origin.x   = r.origin.x;
        t.size.width = r.size.width;
      }

    r = [self frame];
    s = [(NSView *)[self contentView] frame];
    r.size.width  += t.origin.x+t.size.width +2*SPACER-s.size.width;
    r.size.height += t.origin.y+t.size.height+SPACER-s.size.height;
    [self setFrame: r display: NO];
  }

  return self;
}



- (void)timeout_handler: (NSTimer *)timedEntry
{
  NSEvent *nxev = [NSEvent otherEventWithType: NSApplicationDefined
                            location: NSMakePoint (0, 0)
                       modifierFlags: 0
                           timestamp: 0
                        windowNumber: [[NSApp mainWindow] windowNumber]
                             context: [NSApp context]
                             subtype: 0
                               data1: 0
                               data2: 0];

  timer_fired = YES;
  /* We use sto because stopModal/abortModal out of the main loop does not
     seem to work in 10.6.  But as we use stop we must send a real event so
     the stop is seen and acted upon.  */
  [NSApp stop:self];
  [NSApp postEvent: nxev atStart: NO];
}

- (Lisp_Object)runDialogAt: (NSPoint)p
{
  Lisp_Object ret = Qundefined;

  while (popup_activated_flag)
    {
      NSTimer *tmo = nil;
      struct timespec next_time = timer_check ();

      if (timespec_valid_p (next_time))
        {
          double time = timespectod (next_time);
          tmo = [NSTimer timerWithTimeInterval: time
                                        target: self
                                      selector: @selector (timeout_handler:)
                                      userInfo: 0
                                       repeats: NO];
          [[NSRunLoop currentRunLoop] addTimer: tmo
                                       forMode: NSModalPanelRunLoopMode];
        }
      timer_fired = NO;
      dialog_return = Qundefined;
      [NSApp runModalForWindow: self];
      ret = dialog_return;
      if (! timer_fired)
        {
          if (tmo != nil) [tmo invalidate]; /* Cancels timer */
          break;
        }
    }

  if (EQ (ret, Qundefined) && window_closed)
    /* Make close button pressed equivalent to C-g.  */
    Fsignal (Qquit, Qnil);

  return ret;
}

@end


/* ==========================================================================

    Lisp definitions

   ========================================================================== */


DEFUN ("ns-reset-menu", Fns_reset_menu, Sns_reset_menu, 0, 0, 0,
       doc: /* Cause the NS menu to be re-calculated.  */)
     (void)
{
  set_frame_menubar (SELECTED_FRAME (), 1, 0);
  return Qnil;
}


DEFUN ("menu-or-popup-active-p", Fmenu_or_popup_active_p, Smenu_or_popup_active_p, 0, 0, 0,
       doc: /* Return t if a menu or popup dialog is active.  */)
     (void)
{
  return popup_activated () ? Qt : Qnil;
}

/* ==========================================================================

    Lisp interface declaration

   ========================================================================== */

void
syms_of_nsmenu (void)
{
#ifndef NS_IMPL_COCOA
  /* Don't know how to keep track of this in Next/Open/GNUstep.  Always
     update menus there.  */
  trackingMenu = 1;
#endif
  DEFVAR_LISP ("ns-tool-bar-size-mode", Vns_tool_bar_size_mode,
	       doc: /* *Specify the size of the tool bar items.
The value can be `small' (for small items), `regular'
(for regular sized items) and nil for the system default.
The default is nil.

This variable only takes effect for newly created tool bars.
*/);

  Vns_tool_bar_size_mode = Qnil;

  DEFVAR_LISP ("ns-tool-bar-display-mode", Vns_tool_bar_display_mode,
     doc: /* *Specify whether to display the tool bar as icons with
labels.  The value can be `icons' (for icons only), `labels' (for
labels), `both' for both, and nil, in which case the system default is
assumed.  The default is nil.

This variable only takes effect for newly created tool bars.*/);

  Vns_tool_bar_display_mode = Qnil;

  DEFVAR_LISP ("ns-menu-display-services", Vns_menu_display_services,
     doc: /* Service entries are displayed in popup menus if non-nil.*/);

  Vns_menu_display_services = Qt;

  defsubr (&Sns_tool_bar_customize);
  defsubr (&Sns_tool_bar_configuration);
  defsubr (&Sns_reset_menu);
  defsubr (&Smenu_or_popup_active_p);

  DEFSYM (Qdebug_on_next_call, "debug-on-next-call");
  Vcancel_special_indicator_flag = Fcons(Qnil, Qnil);
}
