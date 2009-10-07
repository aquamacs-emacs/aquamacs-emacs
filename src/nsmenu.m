/* NeXT/Open/GNUstep and MacOSX Cocoa menu and toolbar module.
   Copyright (C) 2007, 2008, 2009 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

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
#include "config.h"

#include "lisp.h"
#include "window.h"
#include "buffer.h"
#include "keymap.h"
#include "coding.h"
#include "commands.h"
#include "blockinput.h"
#include "nsterm.h"
#include "termhooks.h"
#include "keyboard.h"

#define NSMENUPROFILE 0

#if NSMENUPROFILE
#include <sys/timeb.h>
#include <sys/types.h>
#endif

#define MenuStagger 10.0

#if 0
int menu_trace_num = 0;
#define NSTRACE(x)        fprintf (stderr, "%s:%d: [%d] " #x "\n",        \
                                __FILE__, __LINE__, ++menu_trace_num)
#else
#define NSTRACE(x)
#endif

#if 0
/* Include lisp -> C common menu parsing code */
#define ENCODE_MENU_STRING(str) ENCODE_UTF_8 (str)
#include "nsmenu_common.c"
#endif

extern Lisp_Object Qundefined, Qmenu_enable, Qmenu_bar_update_hook;
extern Lisp_Object QCtoggle, QCradio;

extern Lisp_Object Vmenu_updating_frame;

Lisp_Object Qdebug_on_next_call;
Lisp_Object Vcancel_special_indicator_flag;
extern Lisp_Object Voverriding_local_map, Voverriding_local_map_menu_flag,
		   Qoverriding_local_map, Qoverriding_terminal_local_map;

extern long context_menu_value;
EmacsMenu *mainMenu, *svcsMenu, *dockMenu;

/* Nonzero means a menu is currently active.  */
static int popup_activated_flag;
static NSModalSession popupSession;
static EmacsAlertPanel *popupSessionAlert;

Lisp_Object Vns_tool_bar_size_mode;
Lisp_Object Vns_tool_bar_display_mode;

/* NOTE: toolbar implementation is at end,
  following complete menu implementation. */


/* ==========================================================================

    Menu: Externally-called functions

   ========================================================================== */


/* FIXME: not currently used, but should normalize with other terms. */
void
x_activate_menubar (struct frame *f)
{
    fprintf (stderr, "XXX: Received x_activate_menubar event.\n");
}


/* Supposed to discard menubar and free storage.  Since we share the
   menubar among frames and update its context for the focused window,
   there is nothing to do here. */
void
free_frame_menubar (struct frame *f)
{
  return;
}


int
popup_activated ()
{
  return popup_activated_flag;
}


/* --------------------------------------------------------------------------
    Update menubar.  Three cases:
    1) deep_p = 0, submenu = nil: Fresh switch onto a frame -- either set up
       just top-level menu strings (OS X), or goto case (2) (GNUstep).
    2) deep_p = 1, submenu = nil: Recompute all submenus.
    3) deep_p = 1, submenu = non-nil: Update contents of a single submenu.
   -------------------------------------------------------------------------- */
void
ns_update_menubar (struct frame *f, int deep_p, EmacsMenu *submenu)
{
  NSAutoreleasePool *pool;
  id menu = [NSApp mainMenu];
  static EmacsMenu *last_submenu = nil;
  BOOL needsSet = NO;
  const char *submenuTitle = [[submenu title] UTF8String];
  extern int waiting_for_input;
  int owfi;
  Lisp_Object items;
  widget_value *wv, *first_wv, *prev_wv = 0;
  int i;

#if NSMENUPROFILE
  struct timeb tb;
  long t;
#endif

  NSTRACE (set_frame_menubar);

  if (f != SELECTED_FRAME ())
      return;
  XSETFRAME (Vmenu_updating_frame, f);
/*fprintf (stderr, "ns_update_menubar: frame: %p\tdeep: %d\tsub: %p\n", f, deep_p, submenu); */

  BLOCK_INPUT;
  pool = [[NSAutoreleasePool alloc] init];

  /* Menu may have been created automatically; if so, discard it. */
  if ([menu isKindOfClass: [EmacsMenu class]] == NO)
    {
      [menu release];
      menu = nil;
    }

  if (menu == nil)
    {
      menu = [[EmacsMenu alloc] initWithTitle: @"Aquamacs"];
      needsSet = YES;
    }
  else
    {  /* close up anything on there */
      id attMenu = [menu attachedMenu];
      if (attMenu != nil)
        [attMenu close];
    }

#if NSMENUPROFILE
  ftime (&tb);
  t = -(1000*tb.time+tb.millitm);
#endif

  /* widget_value is a straightforward object translation of emacs's
     Byzantine lisp menu structures */
  wv = xmalloc_widget_value ();
  wv->name = "Aquamacs";
  wv->value = 0;
  wv->enabled = 1;
  wv->button_type = BUTTON_TYPE_NONE;
  wv->help = Qnil;
  first_wv = wv;

#ifdef NS_IMPL_GNUSTEP
  deep_p = 1; /* until GNUstep NSMenu implements the Panther delegation model */
#endif

  if (deep_p)
    {
      /* Fully parse one or more of the submenus. */
      int n = 0;
      int *submenu_start, *submenu_end;
      int *submenu_top_level_items, *submenu_n_panes;
      struct buffer *prev = current_buffer;
      Lisp_Object buffer;
      int specpdl_count = SPECPDL_INDEX ();
      int previous_menu_items_used = f->menu_bar_items_used;
      Lisp_Object *previous_items
	= (Lisp_Object *) alloca (previous_menu_items_used
				  * sizeof (Lisp_Object));

      /* lisp preliminaries */
      buffer = XWINDOW (FRAME_SELECTED_WINDOW (f))->buffer;
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
      FRAME_MENU_BAR_ITEMS (f) = menu_bar_items (FRAME_MENU_BAR_ITEMS (f));

      /* Now ready to go */
      items = FRAME_MENU_BAR_ITEMS (f);

      /* Save the frame's previous menu bar contents data */
      if (previous_menu_items_used)
	bcopy (XVECTOR (f->menu_bar_vector)->contents, previous_items,
	       previous_menu_items_used * sizeof (Lisp_Object));

      /* parse stage 1: extract from lisp */
      save_menu_items ();

      menu_items = f->menu_bar_vector;
      menu_items_allocated = VECTORP (menu_items) ? ASIZE (menu_items) : 0;
      submenu_start = (int *) alloca (XVECTOR (items)->size * sizeof (int *));
      submenu_end = (int *) alloca (XVECTOR (items)->size * sizeof (int *));
      submenu_n_panes = (int *) alloca (XVECTOR (items)->size * sizeof (int));
      submenu_top_level_items
	= (int *) alloca (XVECTOR (items)->size * sizeof (int *));
      init_menu_items ();
      for (i = 0; i < XVECTOR (items)->size; i += 4)
	{
	  Lisp_Object key, string, maps;

	  key = XVECTOR (items)->contents[i];
	  string = XVECTOR (items)->contents[i + 1];
	  maps = XVECTOR (items)->contents[i + 2];
	  if (NILP (string))
	    break;

          /* FIXME: we'd like to only parse the needed submenu, but this
               was causing crashes in the _common parsing code.. need to make
               sure proper initialization done.. */
/*        if (submenu && strcmp (submenuTitle, SDATA (string)))
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
                  submenuTitle);
	  discard_menu_items ();
	  unbind_to (specpdl_count, Qnil);
          [pool release];
          UNBLOCK_INPUT;
	  return;
        }

      /* parse stage 2: insert into lucid 'widget_value' structures
         [comments in other terms say not to evaluate lisp code here] */
      wv = xmalloc_widget_value ();
      wv->name = "menubar";
      wv->value = 0;
      wv->enabled = 1;
      wv->button_type = BUTTON_TYPE_NONE;
      wv->help = Qnil;
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
            if (!EQ (previous_items[i], XVECTOR (menu_items)->contents[i]))
              if (!(STRINGP (previous_items[i])
                    && STRINGP (XVECTOR (menu_items)->contents[i])
                    && !strcmp (SDATA (previous_items[i]),
                               SDATA (XVECTOR (menu_items)->contents[i]))))
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
              [pool release];
              UNBLOCK_INPUT;
              return;
            }
        }
      /* The menu items are different, so store them in the frame */
      /* FIXME: this is not correct for single-submenu case */
      f->menu_bar_vector = menu_items;
      f->menu_bar_items_used = menu_items_used;

      /* Calls restore_menu_items, etc., as they were outside */
      unbind_to (specpdl_count, Qnil);

      /* Parse stage 2a: now GC cannot happen during the lifetime of the
         widget_value, so it's safe to store data from a Lisp_String */
      wv = first_wv->contents;
      for (i = 0; i < XVECTOR (items)->size; i += 4)
	{
	  Lisp_Object string;
	  string = XVECTOR (items)->contents[i + 1];
	  if (NILP (string))
	    break;
/*           if (submenu && strcmp (submenuTitle, SDATA (string)))
               continue; */

	  wv->name = (char *) SDATA (string);
          update_submenu_strings (wv->contents);
	  wv = wv->next;
	}

      /* Now, update the NS menu; if we have a submenu, use that, otherwise
         create a new menu for each sub and fill it. */
      if (submenu)
        {
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
          [menu fillWithWidgetValue: first_wv->contents];
        }

    }
  else
    {
      static int n_previous_strings = 0;
      static char previous_strings[100][10];
      static struct frame *last_f = NULL;
      int n;
      Lisp_Object string;

      /* Make widget-value tree w/ just the top level menu bar strings */
      items = FRAME_MENU_BAR_ITEMS (f);
      if (NILP (items))
        {
          [pool release];
          UNBLOCK_INPUT;
          return;
        }


      /* check if no change.. this mechanism is a bit rough, but ready */
      n = XVECTOR (items)->size / 4;
      if (f == last_f && n_previous_strings == n)
        {
          for (i = 0; i<n; i++)
            {
	      string = AREF (items, 4*i+1);

              if (EQ (string, make_number (0))) // FIXME: Why???  --Stef
                continue;
              if (NILP (string))
                if (previous_strings[i][0])
                  break;
              else
                continue;
              if (strncmp (previous_strings[i], SDATA (string), 10))
                break;
            }

          if (i == n)
            {
              [pool release];
              UNBLOCK_INPUT;
              return;
            }
        }

      [menu clear];
      for (i = 0; i < XVECTOR (items)->size; i += 4)
	{
	  string = XVECTOR (items)->contents[i + 1];
	  if (NILP (string))
	    break;

          if (n < 100)
            strncpy (previous_strings[i/4], SDATA (string), 10);

	  wv = xmalloc_widget_value ();
	  wv->name = (char *) SDATA (string);
	  wv->value = 0;
	  wv->enabled = 1;
	  wv->button_type = BUTTON_TYPE_NONE;
	  wv->help = Qnil;
	  wv->call_data = (void *) (EMACS_INT) (-1);

#ifdef NS_IMPL_COCOA
          /* we'll update the real copy under app menu when time comes */
          if (!strcmp ("Services", wv->name))
            {
              /* but we need to make sure it will update on demand */
              [svcsMenu setFrame: f];
              [svcsMenu setDelegate: svcsMenu];
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

  [pool release];
  UNBLOCK_INPUT;

}


/* Main emacs core entry point for menubar menus: called to indicate that the
   frame's menus have changed, and the *step representation should be updated
   from Lisp. */
void
set_frame_menubar (struct frame *f, int first_time, int deep_p)
{
  ns_update_menubar (f, deep_p, nil);
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
  if (self = [super initWithTitle: title])
    [self setAutoenablesItems: NO];
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


/* delegate method called when a submenu is being opened: run a 'deep' call
   to set_frame_menubar */
- (void)menuNeedsUpdate: (NSMenu *)menu
{
  /* events may be sent to recently deleted frames,
     FRAME_NS_VIEW (frame) returns 0x0 */
  if (! (frame && FRAME_LIVE_P (frame) && FRAME_NS_VIEW (frame)))
    return;

  NSEvent *event = [[FRAME_NS_VIEW (frame) window] currentEvent];
  /* HACK: Cocoa/Carbon will request update on every keystroke
     via IsMenuKeyEvent -> CheckMenusForKeyEvent.  These are not needed
     since key equivalents are handled through emacs.
     On Leopard, even keystroke events generate SystemDefined events, but
     their subtype is 8. */
  if ([event type] != NSSystemDefined || [event subtype] == 8
      /* Also, don't try this if from an event picked up asynchronously,
         as lots of lisp evaluation happens in ns_update_menubar. */
      || handling_signal != 0)
    return;
/*fprintf (stderr, "Updating menu '%s'\n", [[self title] UTF8String]); NSLog (@"%@\n", event); */
  ns_update_menubar (frame, 1, self);
}


- (BOOL)performKeyEquivalent: (NSEvent *)theEvent
{
  if (SELECTED_FRAME () && FRAME_NS_P (SELECTED_FRAME ())
      && FRAME_NS_VIEW (SELECTED_FRAME ()))
    [FRAME_NS_VIEW (SELECTED_FRAME ()) keyDown: theEvent];
  return YES;
}


/* Parse a widget_value's key rep (examples: 's-p', 's-S', '(C-x C-s)', '<f13>')
   into an accelerator string.  We are only able to display a single character
   for an accelerator, together with an optional modifier combination.  (Under
   Carbon more control was possible, but in Cocoa multi-char strings passed to
   NSMenuItem get ignored.  For now we try to display a super-single letter
   combo, and return the others as strings to be appended to the item title.
   (This is signaled by setting keyEquivModMask to 0 for now.) */
-(NSString *)parseKeyEquiv: (char *)key
{
  char *tpos = key;
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

  if (name_is_separator (wv->name))
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
      /* we display all key equivalents ourselves - Cocoa
	 won't display more than one single key */
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

      [item setTag: (int)wv->call_data];
    }

  return item;
}


/* convenience */
-(void) clear
{
  int n;
  
  for (n = [self numberOfItems]-1; n >= 0; n--)
    {
      NSMenuItem *item = [self itemAtIndex: n];
      NSString *title = [item title];
      if (([title length] == 0 || [@"Aquamacs" isEqualToString: title]
	   || [@"Apple" isEqualToString: title])
          && ![item isSeparatorItem])
        continue;
      [self removeItemAtIndex: n];
    }
}


- (void)fillWithWidgetValue: (void *)wvptr
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
	EmacsMenu *submenu = [[EmacsMenu alloc] initWithTitle: [item title]];
	
	[self setSubmenu: submenu forItem: item];
	[submenu fillWithWidgetValue: wv->contents];
	[submenu release];
	[item setAction: nil];
      }
    }

  [self setMenuChangedMessagesEnabled: YES];
#ifdef NS_IMPL_GNUSTEP
  if ([[self window] isVisible])
    [self sizeToFit];
#else
  if ([self supermenu] == nil)
    [self sizeToFit];
#endif

}


/* adds an empty submenu and returns it */
- (EmacsMenu *)addSubmenuWithTitle: (char *)title forFrame: (struct frame *)f
{
  NSString *titleStr = [NSString stringWithUTF8String: title];
  NSMenuItem *item = [self addItemWithTitle: titleStr
                                     action: nil /*@selector (menuDown:) */
                              keyEquivalent: @""];
  EmacsMenu *submenu = [[EmacsMenu alloc] initWithTitle: titleStr frame: f];
  [self setSubmenu: submenu forItem: item];
  [submenu release];
  return submenu;
}

/* run a menu in popup mode */
- (Lisp_Object)runMenuAt: (NSPoint)p forFrame: (struct frame *)f
                 keymaps: (int)keymaps
{
  EmacsView *view = FRAME_NS_VIEW (f);
/*   p = [view convertPoint:p fromView: nil]; */
  p.y = NSHeight ([view frame]) - p.y;
  NSEvent *e = [[view window] currentEvent];
  NSEvent *event = [NSEvent mouseEventWithType: NSRightMouseDown
                                      location: p
                                 modifierFlags: 0
                                     timestamp: [e timestamp]
                                  windowNumber: [[view window] windowNumber]
                                       context: [e context]
                                   eventNumber: 0/*[e eventNumber] */
                                    clickCount: 1
                                      pressure: 0];
  long retVal;

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

static Lisp_Object
cleanup_popup_menu (Lisp_Object arg)
{
  discard_menu_items ();
  return Qnil;
}


static Lisp_Object
ns_popup_menu (Lisp_Object position, Lisp_Object menu)
{
  EmacsMenu *pmenu;
  struct frame *f = NULL;
  NSPoint p;
  Lisp_Object window, x, y, tem, keymap, title;
  struct gcpro gcpro1;
  int specpdl_count = SPECPDL_INDEX (), specpdl_count2;
  char *error_name = NULL;
  int keymaps = 0;
  widget_value *wv, *first_wv = 0;

  NSTRACE (ns_popup_menu);

  if (!NILP (position))
    {
      check_ns ();
  
      if (EQ (position, Qt)
          || (CONSP (position) && (EQ (XCAR (position), Qmenu_bar)
                                   || EQ (XCAR (position), Qtool_bar))))
        {
          /* Use the mouse's current position.  */
          struct frame *new_f = SELECTED_FRAME ();

          if (FRAME_TERMINAL (new_f)->mouse_position_hook)
            (*FRAME_TERMINAL (new_f)->mouse_position_hook)
              (&new_f, 0, 0, 0, &x, &y, 0);
          if (new_f != 0)
            XSETFRAME (window, new_f);
          else
            {
              window = selected_window;
              x = make_number (0);
              y = make_number (0);
            }
        }
      else
        {
          CHECK_CONS (position);
          tem = Fcar (position);
          if (XTYPE (tem) == Lisp_Cons)
            {
              window = Fcar (Fcdr (position));
              x = Fcar (tem);
              y = Fcar (Fcdr (tem));
            }
          else
            {
              tem = Fcar (Fcdr (position));
              window = Fcar (tem);
              tem = Fcar (Fcdr (Fcdr (tem)));
              x = Fcar (tem);
              y = Fcdr (tem);
            }
        }
  
      CHECK_NUMBER (x);
      CHECK_NUMBER (y);

      if (FRAMEP (window))
        {
          f = XFRAME (window);
      
          p.x = 0;
          p.y = 0;
        }
      else
        {
          struct window *win = XWINDOW (window);
          CHECK_LIVE_WINDOW (window);
          f = XFRAME (WINDOW_FRAME (win));
          p.x = FRAME_COLUMN_WIDTH (f) * WINDOW_LEFT_EDGE_COL (win);
          p.y = FRAME_LINE_HEIGHT (f) * WINDOW_TOP_EDGE_LINE (win);
        }

      p.x += XINT (x); p.y += XINT (y);

      XSETFRAME (Vmenu_updating_frame, f);
    }
  else
    {      /* no position given */
      /* FIXME: if called during dump, we need to stop precomputation of
         key equivalents (see below) because the keydefs in ns-win.el have
         not been loaded yet. */
      if (noninteractive)
        return Qnil;
      Vmenu_updating_frame = Qnil;
    }

  /* now parse the lisp menus */
  record_unwind_protect (unuse_menu_items, Qnil);
  title = Qnil;
  GCPRO1 (title);

  /* Decode the menu items from what was specified.  */

  keymap = get_keymap (menu, 0, 0);
  if (CONSP (keymap))
    {
      /* We were given a keymap.  Extract menu info from the keymap.  */
      Lisp_Object prompt;

      /* Extract the detailed info to make one pane.  */
      keymap_panes (&menu, 1, NILP (position));

      /* Search for a string appearing directly as an element of the keymap.
	 That string is the title of the menu.  */
      prompt = Fkeymap_prompt (keymap);
      title = NILP (prompt) ? build_string ("Select") : prompt;

      /* Make that be the pane title of the first pane.  */
      if (!NILP (prompt) && menu_items_n_panes >= 0)
	XVECTOR (menu_items)->contents[MENU_ITEMS_PANE_NAME] = prompt;

      keymaps = 1;
    }
  else if (CONSP (menu) && KEYMAPP (XCAR (menu)))
    {
      /* We were given a list of keymaps.  */
      int nmaps = XFASTINT (Flength (menu));
      Lisp_Object *maps
	= (Lisp_Object *) alloca (nmaps * sizeof (Lisp_Object));
      int i;

      title = Qnil;

      /* The first keymap that has a prompt string
	 supplies the menu title.  */
      for (tem = menu, i = 0; CONSP (tem); tem = XCDR (tem))
	{
	  Lisp_Object prompt;

	  maps[i++] = keymap = get_keymap (XCAR (tem), 1, 0);

	  prompt = Fkeymap_prompt (keymap);
	  if (NILP (title) && !NILP (prompt))
	    title = prompt;
	}

      /* Extract the detailed info to make one pane.  */
      keymap_panes (maps, nmaps, NILP (position));

      /* Make the title be the pane title of the first pane.  */
      if (!NILP (title) && menu_items_n_panes >= 0)
	XVECTOR (menu_items)->contents[MENU_ITEMS_PANE_NAME] = title;

      keymaps = 1;
    }
  else
    {
      /* We were given an old-fashioned menu.  */
      title = Fcar (menu);
      CHECK_STRING (title);

      list_of_panes (Fcdr (menu));

      keymaps = 0;
    }

  unbind_to (specpdl_count, Qnil);

  /* If no position given, that was a signal to just precompute and cache
     key equivalents, which was a side-effect of what we just did. */
  if (NILP (position))
    {
      discard_menu_items ();
      UNGCPRO;
      return Qnil;
    }

  record_unwind_protect (cleanup_popup_menu, Qnil);
  BLOCK_INPUT;

  /* now parse stage 2 as in ns_update_menubar */
  wv = xmalloc_widget_value ();
  wv->name = "contextmenu";
  wv->value = 0;
  wv->enabled = 1;
  wv->button_type = BUTTON_TYPE_NONE;
  wv->help = Qnil;
  first_wv = wv;

  specpdl_count2 = SPECPDL_INDEX ();

#if 0
  /* FIXME: a couple of one-line differences prevent reuse */
  wv = digest_single_submenu (0, menu_items_used, Qnil);
#else
  {
  widget_value *save_wv = 0, *prev_wv = 0;
  widget_value **submenu_stack
    = (widget_value **) alloca (menu_items_used * sizeof (widget_value *));
/*   Lisp_Object *subprefix_stack
       = (Lisp_Object *) alloca (menu_items_used * sizeof (Lisp_Object)); */
  int submenu_depth = 0;
  int first_pane = 1;
  int i;

  /* Loop over all panes and items, filling in the tree.  */
  i = 0;
  while (i < menu_items_used)
    {
      if (EQ (XVECTOR (menu_items)->contents[i], Qnil))
	{
	  submenu_stack[submenu_depth++] = save_wv;
	  save_wv = prev_wv;
	  prev_wv = 0;
	  first_pane = 1;
	  i++;
	}
      else if (EQ (XVECTOR (menu_items)->contents[i], Qlambda))
	{
	  prev_wv = save_wv;
	  save_wv = submenu_stack[--submenu_depth];
	  first_pane = 0;
	  i++;
	}
      else if (EQ (XVECTOR (menu_items)->contents[i], Qt)
	       && submenu_depth != 0)
	i += MENU_ITEMS_PANE_LENGTH;
      /* Ignore a nil in the item list.
	 It's meaningful only for dialog boxes.  */
      else if (EQ (XVECTOR (menu_items)->contents[i], Qquote))
	i += 1;
      else if (EQ (XVECTOR (menu_items)->contents[i], Qt))
	{
	  /* Create a new pane.  */
	  Lisp_Object pane_name, prefix;
	  char *pane_string;

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
			 ? "" : (char *) SDATA (pane_name));
	  /* If there is just one top-level pane, put all its items directly
	     under the top-level menu.  */
	  if (menu_items_n_panes == 1)
	    pane_string = "";

	  /* If the pane has a meaningful name,
	     make the pane a top-level menu item
	     with its items as a submenu beneath it.  */
	  if (!keymaps && strcmp (pane_string, ""))
	    {
	      wv = xmalloc_widget_value ();
	      if (save_wv)
		save_wv->next = wv;
	      else
		first_wv->contents = wv;
	      wv->name = pane_string;
	      if (keymaps && !NILP (prefix))
		wv->name++;
	      wv->value = 0;
	      wv->enabled = 1;
	      wv->button_type = BUTTON_TYPE_NONE;
	      wv->help = Qnil;
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

	  wv = xmalloc_widget_value ();
	  if (prev_wv)
	    prev_wv->next = wv;
	  else
	    save_wv->contents = wv;
	  wv->name = (char *) SDATA (item_name);
	  if (!NILP (descrip))
	    wv->key = (char *) SDATA (descrip);
	  wv->value = 0;
	  /* If this item has a null value,
	     make the call_data null so that it won't display a box
	     when the mouse is on it.  */
	  wv->call_data
	      = !NILP (def) ? (void *) &XVECTOR (menu_items)->contents[i] : 0;
	  wv->enabled = !NILP (enable);

	  if (NILP (type))
	    wv->button_type = BUTTON_TYPE_NONE;
	  else if (EQ (type, QCtoggle))
	    wv->button_type = BUTTON_TYPE_TOGGLE;
	  else if (EQ (type, QCradio))
	    wv->button_type = BUTTON_TYPE_RADIO;
	  else
	    abort ();

	  wv->selected = !NILP (selected);

          if (! STRINGP (help))
	    help = Qnil;

	  wv->help = help;

	  prev_wv = wv;

	  i += MENU_ITEMS_ITEM_LENGTH;
	}
    }
  }
#endif

  if (!NILP (title))
    {
      widget_value *wv_title = xmalloc_widget_value ();
      widget_value *wv_sep = xmalloc_widget_value ();

      /* Maybe replace this separator with a bitmap or owner-draw item
	 so that it looks better.  Having two separators looks odd.  */
      wv_sep->name = "--";
      wv_sep->next = first_wv->contents;
      wv_sep->help = Qnil;

#ifndef HAVE_MULTILINGUAL_MENU
      if (STRING_MULTIBYTE (title))
	title = ENCODE_MENU_STRING (title);
#endif

      wv_title->name = (char *) SDATA (title);
      wv_title->enabled = NO;
      wv_title->button_type = BUTTON_TYPE_NONE;
      wv_title->help = Qnil;
      wv_title->next = wv_sep;
      first_wv->contents = wv_title;
    }

  pmenu = [[EmacsMenu alloc] initWithTitle:
                               [NSString stringWithUTF8String: SDATA (title)]];
  [pmenu fillWithWidgetValue: first_wv->contents];
  free_menubar_widget_value_tree (first_wv);
  unbind_to (specpdl_count2, Qnil);

  popup_activated_flag = 1;
  tem = [pmenu runMenuAt: p forFrame: f keymaps: keymaps];
  popup_activated_flag = 0;
  [[FRAME_NS_VIEW (SELECTED_FRAME ()) window] makeKeyWindow];

  UNBLOCK_INPUT;
  discard_menu_items ();
  unbind_to (specpdl_count, Qnil);
  UNGCPRO;

  if (error_name) error (error_name);
  return tem;
}




/* ==========================================================================

    Toolbar: externally-called functions

   ========================================================================== */

void
free_frame_tool_bar (FRAME_PTR f)
/* --------------------------------------------------------------------------
    Under NS we just hide the toolbar until it might be needed again.
   -------------------------------------------------------------------------- */
{
  [[FRAME_NS_VIEW (f) toolbar] setVisible: NO];
}

void
update_frame_tool_bar (FRAME_PTR f)
/* --------------------------------------------------------------------------
    Update toolbar contents
   -------------------------------------------------------------------------- */
{
  int i;
  EmacsToolbar *toolbar = [FRAME_NS_VIEW (f) toolbar];

  [toolbar clearActive];
  [toolbar setAllowsUserCustomization:YES];
  [toolbar setAutosavesConfiguration:NO];
  /* problematic, as it creates a defaults file
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
      int img_id;
      struct image *img;
      Lisp_Object image;
      Lisp_Object helpObj;
      Lisp_Object captionObj;
      char *helpText;
      char *captionText;
      char *keyText;
      Lisp_Object label = TOOLPROP (TOOL_BAR_ITEM_CAPTION);
      Lisp_Object key = TOOLPROP (TOOL_BAR_ITEM_KEY);

      if (STRINGP (key))
	keyText = (char *) SDATA (key);
      else if (SYMBOLP (key))
	keyText = (char *) SDATA (SYMBOL_NAME (key) );
      else
	keyText = "?";

      if (strcmp("--", SDATA (label)) == 0)
	[toolbar addDisplayItemSpacerWithIdx: i key: keyText];
      else
	{
      /* If image is a vector, choose the image according to the
	 button state.  */
      image = TOOLPROP (TOOL_BAR_ITEM_IMAGES);
      if (VECTORP (image))
	{
          /* NS toolbar auto-computes disabled and selected images */
          idx = TOOL_BAR_IMAGE_ENABLED_SELECTED;
	  xassert (ASIZE (image) >= idx);
	  image = AREF (image, idx);
	}
      else
        {
          idx = -1;
        }
      /* Ignore invalid image specifications.  */
      if (!valid_image_p (image))
        {
          NSLog (@"Invalid image for toolbar item");
          continue;
        }

      img_id = lookup_image (f, image);
      img = IMAGE_FROM_ID (f, img_id);
      prepare_image_for_display (f, img);

      if (img->load_failed_p || img->pixmap == nil)
        {
          NSLog (@"Could not prepare toolbar image for display.");
          continue;
        }

      helpObj = TOOLPROP (TOOL_BAR_ITEM_HELP);
      if (NILP (helpObj))
        helpObj = TOOLPROP (TOOL_BAR_ITEM_CAPTION);
      helpText = NILP (helpObj) ? "" : (char *)SDATA (helpObj);

      captionObj = TOOLPROP (TOOL_BAR_ITEM_CAPTION);
      captionText = NILP (captionObj) ? "" : (char *)SDATA (captionObj);

      [toolbar addDisplayItemWithImage: img->pixmap idx: i  helpText: helpText
			       enabled: enabled_p  visible: visible_p
				   key: keyText  labelText: captionText];
#undef TOOLPROP
    }
    }

  /* set correct tool-bar height because x_set_window_size can't do it
     before the tool-bar has been drawn. */

  // EmacsView *view = FRAME_NS_VIEW (f);
  // NSWindow *window = [view window];
  // FRAME_NS_TOOLBAR_HEIGHT (f) = 
  //   NSHeight ([window frameRectForContentRect: NSMakeRect (0, 0, 0, 0)])
  //   - FRAME_NS_TITLEBAR_HEIGHT (f);


  if (![toolbar isVisible])
      [toolbar setVisible: YES];

  if ([toolbar changed])
    {
      /* inform app that toolbar has changed */
      NSDictionary *dict = [toolbar configurationDictionary];
      NSMutableDictionary *newDict = [dict mutableCopy];
      NSEnumerator *keys = [[dict allKeys] objectEnumerator];
      NSObject *key;
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

}


DEFUN ("ns-tool-bar-customize", Fns_tool_bar_customize, Sns_tool_bar_customize, 0, 1, "",
       doc: /* View tool bar configuration.
Shows the tool bar customization panel in the given frame. */)
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

  BLOCK_INPUT;
  Lisp_Object item_identifiers = Qnil;
  [[FRAME_NS_VIEW (f) toolbar] setVisible: YES];
  [[FRAME_NS_VIEW (f) toolbar] runCustomizationPalette:FRAME_NS_VIEW (f)];
  UNBLOCK_INPUT;
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

  BLOCK_INPUT;
  Lisp_Object item_identifiers = Qnil;
  NSEnumerator *itemEnum = [[[FRAME_NS_VIEW (f) toolbar] items] reverseObjectEnumerator];
  NSToolbarItem *item = nil;
  while (item = [itemEnum nextObject])
    item_identifiers = Fcons (([item itemIdentifier] == NSToolbarFlexibleSpaceItemIdentifier)
			      ? Qnil : intern ([[item itemIdentifier] UTF8String]),
			      item_identifiers);
  UNBLOCK_INPUT;
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
  self = [super initWithIdentifier: identifier];
  emacsView = view;
  [super setSizeMode: NSTOOLBAR_NEEDED_SIZE_MODE];
  [super setDisplayMode: NSTOOLBAR_NEEDED_DISPLAY_MODE];
  [self setDelegate: self];
  identifierToItem = [[NSMutableDictionary alloc] initWithCapacity: 50];
  activeIdentifiers = [[NSMutableArray alloc] initWithCapacity: 20];
  availableIdentifiers = [[NSMutableArray alloc] initWithCapacity: 50];
  prevEnablement = enablement = 0L;
  return self;
}

- (void)dealloc
{
  [prevIdentifiers release];
  [activeIdentifiers release];
  [availableIdentifiers release];
  [identifierToItem release];
  [super dealloc];
}

- (void) clearActive
{
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
  return [activeIdentifiers isEqualToArray: prevIdentifiers] &&
    enablement == prevEnablement ? NO : YES;
}

- (void) addDisplayItemSpacerWithIdx: (int)idx key: (char *) key
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

  /* 3) update state */
  [identifierToItem setObject: item forKey: identifier];
  [availableIdentifiers addObject: identifier];
  [activeIdentifiers addObject: identifier];
  enablement = (enablement << 1) | false;
}

- (void) addDisplayItemWithImage: (EmacsImage *)img 
			     idx: (int)idx
                        helpText: (char *)help 
			 enabled: (BOOL)enabled
			 visible: (BOOL)visible
			     key: (char *)key
		       labelText: (char *)label;
{
  /* 1) come up w/identifier */
  NSString *identifier = [NSString stringWithCString: key];

  /* 2) create / reuse item */
  NSToolbarItem *item = [identifierToItem objectForKey: identifier];
  if (item == nil)
    {
      item = [[[NSToolbarItem alloc] initWithItemIdentifier: identifier]
               autorelease];
      [item setImage: img];
      [item setToolTip: [NSString stringWithCString: help]];
      [item setLabel: [NSString stringWithCString: label]];
      [item setPaletteLabel: [NSString stringWithCString: label]];
      [item setTarget: emacsView];
      [item setAction: @selector (toolbarClicked:)];
    }

  [item setTag: idx];
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
- (void)validateVisibleItems { }


/* delegate methods */

- (NSToolbarItem *)toolbar: (NSToolbar *)toolbar
      itemForItemIdentifier: (NSString *)itemIdentifier
  willBeInsertedIntoToolbar: (BOOL)flag
{
  /* look up NSToolbarItem by identifier and return... */
  return [identifierToItem objectForKey: itemIdentifier];
}

- (NSArray *)toolbarDefaultItemIdentifiers: (NSToolbar *)toolbar
{
  /* return entire set.. */
  return activeIdentifiers;
}

/* for configuration palette */
- (NSArray *)toolbarAllowedItemIdentifiers: (NSToolbar *)toolbar
{
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
  [textField setBordered: YES];
  [textField setBezeled: YES];
  [textField setDrawsBackground: YES];

  win = [[NSWindow alloc]
            initWithContentRect: [textField frame]
                      styleMask: 0
                        backing: NSBackingStoreBuffered
                          defer: YES];
  [win setReleasedWhenClosed: NO];
  [win setDelegate: self];
  [[win contentView] addSubview: textField];
/*  [win setBackgroundColor: col]; */
  [win setOpaque: NO];

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
  NSRect r = [textField frame];
  NSSize textSize = [str sizeWithAttributes: 
     [NSDictionary dictionaryWithObject: [[textField font] screenFont]
				 forKey: NSFontAttributeName]];
  NSSize padSize = [[[textField font] screenFont] 
		     boundingRectForFont].size;
 
  r.size.width = textSize.width + padSize.width/2;
  r.size.height = textSize.height + padSize.height/2;
  [textField setFrame: r];
  [textField setStringValue: str];
}

- (void) showAtX: (int)x Y: (int)y for: (int)seconds
{
  NSRect wr = [win frame];

  wr.origin = NSMakePoint (x, y);
  wr.size = [textField frame].size;

  [win setFrame: wr display: YES];
  [win orderFront: self];
  [win display];
  timer = [NSTimer scheduledTimerWithTimeInterval: (float)seconds target: self
                                         selector: @selector (hide)
                                         userInfo: nil repeats: NO];
  [timer retain];
}

- (void) hide
{
  [win close];
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



/* ==========================================================================

    Popup Dialog: implementing functions

   ========================================================================== */


static Lisp_Object
pop_down_menu (Lisp_Object arg)
{
  struct Lisp_Save_Value *p = XSAVE_VALUE (arg);
  if (popup_activated_flag)
    {
      popup_activated_flag = 0;
      BLOCK_INPUT;
      [NSApp endModalSession: popupSession];
      [NSApp endSheet:[popupSessionAlert window]];
      [popupSessionAlert release];
      //[[((EmacsAlertPanel *) (p->pointer)) window] close];
      [[FRAME_NS_VIEW (SELECTED_FRAME ()) window] makeKeyWindow];
      UNBLOCK_INPUT;
    }
  return Qnil;
}


Lisp_Object
ns_popup_dialog (Lisp_Object position, Lisp_Object contents, Lisp_Object header)
{
  EmacsAlertPanel *dialog;
  Lisp_Object window, tem=Qnil;
  struct frame *f;
  NSPoint p;
  BOOL isQ;

  NSTRACE (x-popup-dialog);
  
  check_ns ();

  CHECK_CONS (contents);

  isQ = NILP (header);

  if (EQ (position, Qt)
      || (CONSP (position) && (EQ (XCAR (position), Qmenu_bar)
                               || EQ (XCAR (position), Qtool_bar))))
    {
      window = selected_window;
    }
  else if (CONSP (position))
    {
      Lisp_Object tem;
      tem = Fcar (position);
      if (XTYPE (tem) == Lisp_Cons)
        window = Fcar (Fcdr (position));
      else
        {
          tem = Fcar (Fcdr (position));  /* EVENT_START (position) */
          window = Fcar (tem);	     /* POSN_WINDOW (tem) */
        }
    }
  else if (WINDOWP (position) || FRAMEP (position))
    {
      window = position;
    }
  else
    window = Qnil;

  if (FRAMEP (window))
    f = XFRAME (window);
  else if (WINDOWP (window))
    {
      CHECK_LIVE_WINDOW (window);
      f = XFRAME (WINDOW_FRAME (XWINDOW (window)));
    }
  else
    CHECK_WINDOW (window);

  p.x = (int)f->left_pos + ((int)FRAME_COLUMN_WIDTH (f) * f->text_cols)/2;
  p.y = (int)f->top_pos + (FRAME_LINE_HEIGHT (f) * f->text_lines)/2;

  BLOCK_INPUT;
  dialog = [[EmacsAlertPanel alloc] init];

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
    int specpdl_count = SPECPDL_INDEX ();
    record_unwind_protect (pop_down_menu, make_save_value (dialog, 0));
    popup_activated_flag = 1;

  extern EMACS_TIME timer_check (int do_it_now);  

  [dialog beginSheetModalForWindow:[FRAME_NS_VIEW (f) window]
		     modalDelegate:dialog
		    didEndSelector:@selector(alertDidEnd:returnCode:contextInfo:)
		       contextInfo:&ret];
    

  
  /* initiate a session that will be ended by pop_down_menu */
  popupSessionAlert = [dialog retain];  
  popupSession = [NSApp beginModalSessionForWindow: [dialog window]];
  
  
  while (popup_activated_flag
	 && ret == -1
         && ([NSApp runModalSession: popupSession]
	     == NSRunContinuesResponse))
    {
      /* Run this for timers.el, indep of atimers; might not return.
         TODO: use return value to avoid calling every iteration. */
      timer_check (1);
      [NSThread sleepUntilDate: [NSDate dateWithTimeIntervalSinceNow: 0.1]];
    }

  if (ret>=0 && ret<dialog->returnValueCount)
    {
      // *(EMACS_INT*)(&tem)
      tem = (Lisp_Object) dialog->returnValues[ret];
      if ([[dialog suppressionButton] state] == NSOnState)
	{
	  tem = Fcons (tem, dialog->returnValues[[[dialog suppressionButton] tag]]);
	}
    }
    unbind_to (specpdl_count, Qnil);  /* calls pop_down_menu */
  }
  UNBLOCK_INPUT;
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
	      if (STRINGP ( XCDR (XCAR (item))))
		key =  [NSString stringWithUTF8String: SDATA (XCDR (XCAR (item)))];
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
	    [button setKeyEquivalent: key];

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

    Lisp definitions

   ========================================================================== */


DEFUN ("ns-reset-menu", Fns_reset_menu, Sns_reset_menu, 0, 0, 0,
       doc: /* Cause the NS menu to be re-calculated.  */)
     ()
{
  set_frame_menubar (SELECTED_FRAME (), 1, 0);
  return Qnil;
}


DEFUN ("x-popup-menu", Fx_popup_menu, Sx_popup_menu, 2, 2, 0,
       doc: /* Pop up a deck-of-cards menu and return user's selection.
POSITION is a position specification.  This is either a mouse button event
or a list ((XOFFSET YOFFSET) WINDOW)
where XOFFSET and YOFFSET are positions in pixels from the top left
corner of WINDOW.  (WINDOW may be a window or a frame object.)
This controls the position of the top left of the menu as a whole.
If POSITION is t, it means to use the current mouse position.

MENU is a specifier for a menu.  For the simplest case, MENU is a keymap.
The menu items come from key bindings that have a menu string as well as
a definition; actually, the \"definition\" in such a key binding looks like
\(STRING . REAL-DEFINITION).  To give the menu a title, put a string into
the keymap as a top-level element.

If REAL-DEFINITION is nil, that puts a nonselectable string in the menu.
Otherwise, REAL-DEFINITION should be a valid key binding definition.

You can also use a list of keymaps as MENU.
  Then each keymap makes a separate pane.

When MENU is a keymap or a list of keymaps, the return value is the
list of events corresponding to the user's choice. Note that
`x-popup-menu' does not actually execute the command bound to that
sequence of events.

Alternatively, you can specify a menu of multiple panes
  with a list of the form (TITLE PANE1 PANE2...),
where each pane is a list of form (TITLE ITEM1 ITEM2...).
Each ITEM is normally a cons cell (STRING . VALUE);
but a string can appear as an item--that makes a nonselectable line
in the menu.
With this form of menu, the return value is VALUE from the chosen item.  */)
     (position, menu)
     Lisp_Object position, menu;
{
  return ns_popup_menu (position, menu);
}


DEFUN ("x-popup-dialog", Fx_popup_dialog, Sx_popup_dialog, 2, 3, 0,
       doc: /* Pop up a dialog box and return user's selection.
POSITION specifies which frame to use.
This is normally a mouse button event or a window or frame.
If POSITION is t, it means to use the frame the mouse is on.
The dialog box appears in the middle of the specified frame.

CONTENTS specifies the alternatives to display in the dialog box.
It is a list of the form (DIALOG ITEM1 ITEM2...).
Each ITEM is a cons cell (STRING . VALUE).
The return value is VALUE from the chosen item.

In Aquamacs, STRING may be a title string, or of the form 
(TITLE . KEYSTRING), where TITLE is a string indicating the
button title, and KEYSTRING a one-letter string giving the
key equivalent for the button. 

An ITEM may also be just a string--that makes a nonselectable item.
An ITEM may also be nil--that means to put all preceding items
on the left of the dialog box and all following items on the right.

In Aquamacs, if VALUE is `suppress', the button will be shown as a 
checkbox which can be selected in addition to any of the other buttons
except Cancel. In this case, the return value is a cons cell of the
form (VALUE . suppress).

In Aquamacs, an ITEM may be `cancel' to insert a cancel button.  If there
is an ITEM `no-cancel', no cancel button will be inserted at all;
if there is no such item, a default cancel button will be inserted.

The order of buttons in the dialog follows system conventions; the
default button should be specified first in the list of ITEMs.

If HEADER is non-nil, the frame title for the box is "Information",
otherwise it is "Question".  HEADER is unused in Aquamacs.

If the user gets rid of the dialog box without making a valid choice,
for instance using the window manager or using a cancel button,
then this produces a quit and `x-popup-dialog' does not return.  */)
     (position, contents, header)
     Lisp_Object position, contents, header;
{
  return ns_popup_dialog (position, contents, header);
}

DEFUN ("menu-or-popup-active-p", Fmenu_or_popup_active_p, Smenu_or_popup_active_p, 0, 0, 0,
       doc: /* Return t if a menu or popup dialog is active.  */)
     ()
{
  return popup_activated () ? Qt : Qnil;
}

/* ==========================================================================

    Lisp interface declaration

   ========================================================================== */

void
syms_of_nsmenu ()
{
  DEFVAR_LISP ("ns-tool-bar-size-mode", &Vns_tool_bar_size_mode,
	       doc: /* *Specify the size of the tool bar items.
The value can be `small' (for small items), `regular' 
(for regular sized items) and nil for the system default.
The default is nil.

This variable only takes effect for newly created tool bars.
*/);

  Vns_tool_bar_size_mode = Qnil;

  DEFVAR_LISP ("ns-tool-bar-display-mode", &Vns_tool_bar_display_mode,
     doc: /* *Specify whether to display the tool bar as icons with
labels.  The value can be `icons' (for icons only), `labels' (for
labels), `both' for both, and nil, in which case the system default is
assumed.  The default is nil.

This variable only takes effect for newly created tool bars.*/);

  Vns_tool_bar_display_mode = Qnil;

  defsubr (&Sns_tool_bar_customize);
  defsubr (&Sns_tool_bar_configuration);
  defsubr (&Sx_popup_menu);
  defsubr (&Sx_popup_dialog);
  defsubr (&Sns_reset_menu);
  defsubr (&Smenu_or_popup_active_p);
  staticpro (&menu_items);
  menu_items = Qnil;

  Qdebug_on_next_call = intern ("debug-on-next-call");
  staticpro (&Qdebug_on_next_call);
  Vcancel_special_indicator_flag = Fcons(Qnil, Qnil);
}

// arch-tag: 75773656-52e5-4c44-a398-47bd87b32619
