/* Functions added for Aquamacs to GNU Emacs.

   Copyright (C) 2004-2023.

   This file is part of GNU Emacs.

   Aquamacs Emacs is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or (at
   your option) any later version.

   Aquamacs Emacs is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Aquamacs Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

/*
  New functions for Aquamacs. In earlier versions of Aquamacs, these
  were in nsfns.m.
*/

/* This should be the first include, as it may set up #defines affecting
   interpretation of even the system includes.  */
#include <config.h>

#include <math.h>
#include <c-strcase.h>

#include "lisp.h"
#include "blockinput.h"
#include "nsterm.h"
#include "nsaquamacs.h"

#include "window.h"
#include "character.h"
#include "buffer.h"
#include "keyboard.h"
#include "termhooks.h"
#include "fontset.h"
#include "font.h"

/* Menu declarations. */
NSMenu *panelMenu;

// Menu definition functions

void
aquamacs_make_panel_menu()
{
  /* set up the panel menu (while panels are shown)
     must provide Edit functions. */
  NSMenuItem *item;

  NSMenu *panelAppMenu = [[EmacsMenu alloc] initWithTitle: @"Aquamacs"];
  [panelAppMenu setAutoenablesItems: NO];
  [panelAppMenu insertItemWithTitle: @"Hide Aquamacs Emacs"
                             action: @selector (hide:)
                      keyEquivalent: @"h"
                            atIndex: 0];
  item =  [panelAppMenu insertItemWithTitle: @"Hide Others"
                                     action: @selector (hideOtherApplications:)
                              keyEquivalent: @"h"
                                    atIndex: 1];
  [item setKeyEquivalentModifierMask: NSCommandKeyMask | NSAlternateKeyMask];
  /* app menu is not available (except hide, really) */

  panelMenu = [[[NSMenu alloc] initWithTitle: @"Aquamacs*"] retain];

  item = [panelMenu insertItemWithTitle: ns_app_name
                                 action: @selector (menuDown:)
                          keyEquivalent: @""
                                atIndex: 0];
  [panelMenu setSubmenu: panelAppMenu forItem: item];

  NSMenu *submenu = [[NSMenu alloc] initWithTitle:NSLocalizedString(@"Edit", @"The Edit menu")];
  [panelMenu setSubmenu:submenu forItem:[panelMenu addItemWithTitle:@"Edit" action:NULL keyEquivalent:@""]];
  [submenu addItemWithTitle:NSLocalizedString(@"Undo", nil)
                     action:@selector(undo:)
              keyEquivalent:@"z"];

  [submenu addItemWithTitle:NSLocalizedString(@"Redo", nil)
                     action:@selector(redo:)
              keyEquivalent:@"Z"];

  [submenu addItem:[NSMenuItem separatorItem]];

  [submenu addItemWithTitle:NSLocalizedString(@"Cut", nil)
                     action:@selector(cut:)
              keyEquivalent:@"x"];

  [submenu addItemWithTitle:NSLocalizedString(@"Copy", nil)
                     action:@selector(copy:)
              keyEquivalent:@"c"];

  [submenu addItemWithTitle:NSLocalizedString(@"Paste", nil)
                     action:@selector(paste:)
              keyEquivalent:@"v"];

  [submenu addItemWithTitle:NSLocalizedString(@"Delete", nil)
                     action:@selector(delete:)
              keyEquivalent:@""];

  [submenu addItemWithTitle: NSLocalizedString(@"Select All", nil)
                     action: @selector(selectAll:)
              keyEquivalent: @"a"];

}

void
aquamacs_set_edit_menu(NSMenu *appMenu)
{
  NSMenu *editMenu = [[NSMenu alloc]
                       initWithTitle:NSLocalizedString(@"Edit",
                                                       @"The Edit menu")];
  [appMenu setSubmenu:editMenu
              forItem:[panelMenu addItemWithTitle:@"Edit"
                                           action:NULL keyEquivalent:@""]];
  [editMenu addItemWithTitle:NSLocalizedString(@"Undo", nil)
                      action:@selector(undo:)
               keyEquivalent:@"z"];

  [editMenu addItemWithTitle:NSLocalizedString(@"Redo", nil)
                      action:@selector(redo:)
               keyEquivalent:@"Z"];

  [editMenu addItem:[NSMenuItem separatorItem]];

  [editMenu addItemWithTitle:NSLocalizedString(@"Cut", nil)
                      action:@selector(cut:)
               keyEquivalent:@"x"];

  [editMenu addItemWithTitle:NSLocalizedString(@"Copy", nil)
                      action:@selector(copy:)
               keyEquivalent:@"c"];

  [editMenu addItemWithTitle:NSLocalizedString(@"Paste", nil)
                      action:@selector(paste:)
               keyEquivalent:@"v"];

  [editMenu addItemWithTitle:NSLocalizedString(@"Delete", nil)
                      action:@selector(delete:)
               keyEquivalent:@""];

  [editMenu addItemWithTitle: NSLocalizedString(@"Select All", nil)
                      action: @selector(selectAll:)
               keyEquivalent: @"a"];
}


// For PDFView
#import <Quartz/Quartz.h>
#import <WebKit/WebKit.h>

static NSScreen *
ns_get_screen (Lisp_Object screen)
{
  struct frame *f;
  struct terminal *terminal;

  if (EQ (Qt, screen)) /* not documented */
    return [NSScreen mainScreen];

  terminal = decode_live_terminal (screen);
  if (terminal->type != output_ns)
    return NULL;

  if (NILP (screen))
    f = SELECTED_FRAME ();
  else if (FRAMEP (screen))
    f = XFRAME (screen);
  else
    {
      struct ns_display_info *dpyinfo = terminal->display_info.ns;
      f = dpyinfo->ns_focus_frame
        ? dpyinfo->ns_focus_frame : dpyinfo->highlight_frame;
    }

  return ((f && FRAME_NS_P (f)) ? [[FRAME_NS_VIEW (f) window] screen]
	  : NULL);
}

DEFUN ("ns-popup-page-setup-panel", Fns_popup_page_setup_panel, Sns_popup_page_setup_panel,
       0, 0, "",
       doc: /* Pop up the page setup panel.  */)
  (void)
{
  check_window_system (NULL);
  block_input();

  NSPageLayout *pageLayout = [NSPageLayout pageLayout];

  [pageLayout beginSheetWithPrintInfo:[NSPrintInfo sharedPrintInfo]
		       modalForWindow:[FRAME_NS_VIEW (SELECTED_FRAME ()) window] /* not right. */
			     delegate:FRAME_NS_VIEW (SELECTED_FRAME ())
		       didEndSelector:@selector(pageLayoutDidEnd:returnCode:contextInfo:)
			  contextInfo:nil];

  /* runModal doesn't work for some reason, even though
     it would be the right thing to do.  Use the technique from ns_popup_dialog?
     can't get at the pageLayout window, which we'd need for that. */

  // [pageLayout runModal];

  // [[FRAME_NS_VIEW (SELECTED_FRAME ()) window] makeKeyWindow];
  unblock_input();
  return Qnil;
}

DEFUN ("aquamacs-html-to-rtf", Faquamacs_html_to_rtf,
       Saquamacs_html_to_rtf, 1, 1, 0,
       doc: /* Converts HTML to RTF.
               Available in Aquamacs only. */)
  (Lisp_Object str)
{
  Lisp_Object result = Qnil;
  /* convert HTML to RTF for formatting */
  NSData *htmlData = [[NSString stringWithUTF8String: SDATA (str)]
		       dataUsingEncoding:NSUTF8StringEncoding];

  NSAttributedString *attrString = [[NSAttributedString alloc]
					   initWithHTML:htmlData
						options:@{NSTextEncodingNameDocumentOption: @"UTF-8"}
  documentAttributes:NULL];

  if (attrString)
    {
      NSData *rtfData = [attrString RTFFromRange: NSMakeRange(0,[attrString length])
			      documentAttributes: @{NSDocumentTypeDocumentAttribute: NSRTFTextDocumentType}];
      if (rtfData)
	{
	  if ([rtfData length] > 0)
	    {
	      result = make_string_from_bytes ((char *) [rtfData bytes], 1, [rtfData length]);
	    }
	  else
	    result = Qnil;
	}
      else
	error ("aquamacs-html-to-rtf: Generating RTF failed.");
    }
  else
    error ("aquamacs-html-to-rtf: Parsing HTML failed.");
  return result;
}


DEFUN ("aquamacs-render-to-pdf", Faquamacs_render_to_pdf, Saquamacs_render_to_pdf,
       0, 3, "",
       doc: /* Render HTML buffer SOURCE to PDF.
               If successful, resulting PDF (and the input HTML) are put
               on the pasteboard.*/)
  (source, width, height)
  Lisp_Object source, width, height;
{
  struct frame *f;
  check_window_system (NULL);
  CHECK_FIXNAT(width);
  CHECK_FIXNAT(height);
  if (! BUFFERP (source))
    {
      error ("Must give buffer as source for aquamacs-render-to-pdf.");
    }

  block_input();

  WebView *htmlPage = [[WebView alloc] initWithFrame:NSMakeRect(0,0,XFIXNUM (width),XFIXNUM (height))
					   frameName:@"myFrame"
					   groupName:@"myGroup"];

  /* Render HTML */
  struct buffer *old_buffer = NULL;
  if (XBUFFER (source) != current_buffer)
    {
      old_buffer = current_buffer;
      set_buffer_internal_1 (XBUFFER (source));
    }
  Lisp_Object string = make_buffer_string (BEGV, ZV, 0);
  if (old_buffer)
    set_buffer_internal_1 (old_buffer);

  [[htmlPage mainFrame] loadHTMLString:
	[NSString stringWithUTF8String: SDATA (string)] /* is copied */
			       baseURL:[NSURL fileURLWithPath: [[NSBundle mainBundle] resourcePath]]];

  /* In this case, let's just wait until it's finished. */
  double current_time = [[NSDate date] timeIntervalSinceReferenceDate];
  while ([htmlPage  estimatedProgress] > 0.00) {
    if ([[NSDate date] timeIntervalSinceReferenceDate] - current_time >= .6)
      break;
    [[NSRunLoop currentRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.02]];
  }
  if ([htmlPage  estimatedProgress] > 0.00)
    {
      unblock_input();
      [htmlPage release];
      error ("Rendering failed (timeout).");
    }

  //get the rect for the rendered frame
  NSRect webFrameRect = [[[[htmlPage mainFrame] frameView] documentView] frame];
  //get the rect of the current webview
  NSRect webViewRect = [htmlPage frame];

  //calculate the new frame
  NSRect newWebViewRect = NSMakeRect(webViewRect.origin.x,
                                     webViewRect.origin.y - (NSHeight(webFrameRect) - NSHeight(webViewRect)),
				     NSWidth(webViewRect),
				     NSHeight(webFrameRect));
  //set the frame
  [htmlPage setFrame:newWebViewRect];

  NSRect bounds = [[[[htmlPage mainFrame]frameView]documentView]
                    bounds];

  /* Alternative way of doing this, via Javascript ...
     NSString *actualHeightStr = [htmlPage stringByEvaluatingJavaScriptFromString:@"(function(){var a=document.body,b=document.documentElement;return Math.max(a.scrollHeight,b.scrollHeight)})();"];
     int actualHeight = [actualHeightStr integerValue];
     NSString *actualWidthStr = [htmlPage stringByEvaluatingJavaScriptFromString:@"(function(){var a=document.body,b=document.documentElement;return Math.max(a.scrollWidth,b.scrollWidth)})();"];
     int actualWidth = [actualWidthStr integerValue];
     NSLog(actualWidthStr);
     NSLog(actualHeightStr);
     if (actualHeight > 0) // JS above worked as intended
     {
     bounds.size.height = actualHeight;
     }
     if (actualWidth > 0) // JS above worked as intended
     {
     bounds.size.width = actualWidth;
     }

  */

  /* Note: we could also just use writePDFInsideRect:toPasteboard:
     but we're concurrently writing HTML as well. */

  NSData *viewImageData=[[[[htmlPage mainFrame] frameView] documentView]
			  dataWithPDFInsideRect:bounds];
  PDFDocument *pdf = [[PDFDocument alloc] initWithData:viewImageData];
  NSPasteboard *pasteboard = [NSPasteboard generalPasteboard];
  [pasteboard clearContents];
  [pasteboard declareTypes:[NSArray arrayWithObjects:NSPDFPboardType, NSHTMLPboardType, nil] owner:nil];
  [pasteboard setData:[pdf dataRepresentation] forType:NSPDFPboardType];
  [pasteboard setData:[NSData dataWithBytes: SDATA (string) length:strlen(SDATA (string))] forType:NSHTMLPboardType];
  [pdf release];
  [htmlPage release];
  unblock_input();
  return Qnil;
}

DEFUN ("ns-popup-print-panel", Fns_popup_print_panel, Sns_popup_print_panel,
       0, 2, "",
       doc: /* Pop up the print panel.  */)
  (frame, source)
  Lisp_Object frame, source;
{
  struct frame *f;
  check_window_system (NULL);
  block_input();
  if (NILP (frame))
    f = SELECTED_FRAME ();
  else
    {
      CHECK_FRAME (frame);
      f = XFRAME (frame);
    }

  WebView *htmlPage = [[WebView alloc] initWithFrame:NSMakeRect(0,0,300,300)
					   frameName:@"myFrame"
					   groupName:@"myGroup"];


  if (STRINGP (source))
    {
      [[htmlPage mainFrame] loadRequest:[NSURLRequest requestWithURL:
					      [NSURL fileURLWithPath:
						       [NSString stringWithUTF8String: SDATA (source) ]]]];
    }
  else if (BUFFERP (source))
    {
      struct buffer *old_buffer = NULL;
      if (XBUFFER (source) != current_buffer)
	{
	  old_buffer = current_buffer;
	  set_buffer_internal_1 (XBUFFER (source));
	}
      Lisp_Object string = make_buffer_string (BEGV, ZV, 0);
      if (old_buffer)
        set_buffer_internal_1 (old_buffer);

      [[htmlPage mainFrame] loadHTMLString:
	    [NSString stringWithUTF8String: SDATA (string)] /* is copied */
				   baseURL:[NSURL fileURLWithPath: [[NSBundle mainBundle] resourcePath]]];
    }
  else
    {
      unblock_input();
      error ("Must give buffer or file path as source for ns-popup-print-panel.");
    }

  /*

    works for PDF:

    PDFView *pdfView = [[[PDFView alloc] init] retain];
    PDFDocument *pdfDoc = [[[PDFDocument alloc] initWithURL: [NSURL fileURLWithPath:
    [NSString stringWithUTF8String: SDATA (pdf_file) ]]] retain];

    if (pdfDoc == NULL)
    {
    [pdfView release];
    error("Could not load PDF file.");
    }


    [pdfView setDocument: pdfDoc];
    [pdfView setDisplayMode: kPDFDisplaySinglePageContinuous];
    [pdfView layoutDocumentView];

    [FRAME_NS_VIEW(f) addSubview:pdfView];
    // this seems to have problems with the run loop or something
    [pdfView printWithInfo:[NSPrintInfo sharedPrintInfo] autoRotate:NO];

  */

  /* call back when finished loading.
     delegate implemented in nsterm.m */
  [htmlPage setFrameLoadDelegate:FRAME_NS_VIEW (f)];

  unblock_input();
  return Qnil;
}

// extern void ns_update_menubar (struct frame *f, bool deep_p, EmacsMenu *submenu);

Lisp_Object save_panel_callback;
DEFUN ("ns-popup-save-panel", Fns_popup_save_panel, Sns_popup_save_panel,
       0, 3, "",
       doc: /* Pop up the save panel as a sheet over the current buffer.
               Upon completion, the event `ns-save-panel-closed' will be sent,
               with the variable `ns-save-panel-file' containing the selected file
               (nil if cancelled), and `ns-save-panel-buffer' the buffer current
               when `ns-popup-save-panel' was called.
            */)
  (prompt, dir, init)
  Lisp_Object prompt, dir, init;
{
  NSSavePanel *panel;

  check_window_system (NULL);
  block_input();

  NSString *promptS = NILP (prompt) || !STRINGP (prompt) ? nil :
    [NSString stringWithUTF8String: SDATA (prompt)];
  NSString *dirS = NILP (dir) || !STRINGP (dir) ?
    [NSString stringWithUTF8String: SDATA (BVAR (current_buffer, directory))] :
    [NSString stringWithUTF8String: SDATA (dir)];
  NSString *initS = NILP (init) || !STRINGP (init) ? nil :
    [NSString stringWithUTF8String: SDATA (init)];


  if ([dirS characterAtIndex: 0] == '~')
    dirS = [dirS stringByExpandingTildeInPath];

  panel = [EmacsSavePanel savePanel];

  [panel setTitle: promptS];

  [panel setTreatsFilePackagesAsDirectories: YES];
  [panel setCanSelectHiddenExtension:NO];
  [panel setExtensionHidden:NO];

  if (dirS) [panel setDirectoryURL: [NSURL fileURLWithPath: dirS]];
  if (initS) [panel setNameFieldStringValue: [initS lastPathComponent]];

  [panel beginSheetModalForWindow:[FRAME_NS_VIEW (SELECTED_FRAME ()) window]
		completionHandler:
	   ^(NSInteger result) {
      [((EmacsApp *) NSApp) savePanelDidEnd2: panel returnCode:result contextInfo:current_buffer];

    }];
  // to do: move code from savePanelDidEnd2 here

  set_frame_menubar (SELECTED_FRAME (), 0);
  unblock_input();
  return Qnil;
}

DEFUN ("ns-os-version", Fns_os_version, Sns_os_version, 0, 0, 0,
       doc: /* Return the version identification of the OS.
               This is a human-readable string inappropriate for parsing.
               See `x-server-version' for programmatical uses.*/)
  (void)
{
#ifdef NS_IMPL_GNUSTEP
  return Qnil;
#else
  NSString *operatingSystemVersionString;
  operatingSystemVersionString = [[NSProcessInfo processInfo]
                                   operatingSystemVersionString];
  return build_string([operatingSystemVersionString UTF8String]);
#endif
}

// -- ODB Editor Support ----------------------------------------------------

/*
 * Code adapted from MacVIM - gui_macvim.m
 *
 * The ODB Editor protocol works like this:
 * - An external program (the server) asks us to open a file and associates
 *   three things with this file: (1) a server id (a four character code that
 *   identifies the server), (2) a path that can be used as window title for
 *   the file (optional), (3) an arbitrary token (optional)
 *   This is handled in nsterm.m's extractArgumentsFromOdocEvent().
 * - When a file is saved or closed, we should tell the server about which
 *   file was modified and also pass back the token
 *
 * All communication between Aquamacs and the server is handled via Apple Events.
 */



OSStatus odb_event (struct buffer *buffer,
                    Lisp_Object parms,
                    const AEEventID action)
{
  Lisp_Object Qremote_id = intern("remote-id");
  Lisp_Object Qremote_token_data = intern("remote-token-data");
  Lisp_Object Qremote_token_type = intern("remote-token-type");

  Lisp_Object remote_id, remote_token_data, remote_token_type;
  OSType rid;

  remote_id = Fcdr (Fassq (Qremote_id, parms));
  remote_token_data = Fcdr (Fassq (Qremote_token_data, parms));
  remote_token_type = Fcdr (Fassq (Qremote_token_type, parms));

  if (NILP (remote_id))
    return -1000;

  if (Fcdr (remote_id))
    rid = (XUFIXNUM (Fcar (remote_id)) << 16) | XUFIXNUM (Fcdr (remote_id));
  else
    rid = XUFIXNUM (remote_id);

  /* Destination Process */
  NSAppleEventDescriptor *targetDesc = [NSAppleEventDescriptor
                                         descriptorWithDescriptorType:typeApplSignature
                                                                bytes:&rid
                                                               length:sizeof(OSType)];


  /* file name */
  NSString *path = [NSString stringWithUTF8String:SDATA (BVAR (current_buffer, filename))];
  NSData *pathData = [[[NSURL fileURLWithPath:path] absoluteString]
                       dataUsingEncoding:NSUTF8StringEncoding];
  NSAppleEventDescriptor *pathDesc = [NSAppleEventDescriptor
                                       descriptorWithDescriptorType:typeFileURL data:pathData];

  /* Make event */
  NSAppleEventDescriptor *event = [NSAppleEventDescriptor
                                    appleEventWithEventClass:kODBEditorSuite
                                                     eventID:action
                                            targetDescriptor:targetDesc
                                                    returnID:kAutoGenerateReturnID
                                               transactionID:kAnyTransactionID];

  [event setParamDescriptor:pathDesc forKeyword:keyDirectObject];

  if (! NILP (remote_token_data) && ! NILP (remote_token_type))
    {
      NSData *tokenData = [[NSString stringWithUTF8String:SDATA (remote_token_data)]
			      dataUsingEncoding:NSNonLossyASCIIStringEncoding];
      if (tokenData)
        {
          DescType tokenType;

          if (Fcdr (remote_token_type))
            tokenType = (XUFIXNUM (Fcar (remote_token_type)) << 16) | XUFIXNUM (Fcdr (remote_token_type));
          else
            tokenType = XUFIXNUM (remote_token_type);

          [event setParamDescriptor:
                   [NSAppleEventDescriptor descriptorWithDescriptorType:tokenType
                                                                   data:tokenData]
                         forKeyword: keySenderToken];
        }
    }

  return AESendMessage([event aeDesc], NULL, kAENoReply | kAENeverInteract,
                       kAEDefaultTimeout);
}


DEFUN ("ns-send-odb-notification", Fns_send_odb_notification, Sns_send_odb_notification, 3, 3, 0,
       doc: /* Send ODB notification after file save.
               BUF is the buffer in question.
               TYPE is one of `saved', `closed'.
               PARMS is an association list as communicated for the opening event for the specific buffer.
            */)
  (type, buf, parms)
  Lisp_Object type, buf, parms;
{
  struct buffer *buffer;

  if (NILP (buf))
    buffer = current_buffer;
  else
    buffer = XBUFFER (buf);

  CHECK_BUFFER (buf);

  if (EQ (type, intern ("closed")))
    {
      OSStatus err_val = odb_event (buffer, parms, kAEClosedFile);
      if (err_val != noErr)
	error("Error %d during ODB notification for `closed'.", err_val);
    }
  else if (EQ (type, intern ("saved")))
    {
      OSStatus err_val = odb_event (buffer, parms, kAEModifiedFile);
      if (err_val != noErr)
	error("Error %d during ODB notification for `saved'.", err_val);
    }
  else
    {
      error ("ODB: TYPE must be one of `closed', `saved'.");
    }

  return Qnil;
}

DEFUN ("ns-application-hidden-p", Fns_application_hidden_p, Sns_application_hidden_p, 0, 0, 0,
       doc: /* Returns non-nil if application is hidden. */)
  (void)
{

  check_window_system (NULL);
  return ([NSApp isHidden] == YES ?
	  Qt : Qnil);
}

DEFUN ("ns-launch-URL-with-default-browser", Fns_launch_url_with_default_browser, Sns_launch_url_with_default_browser, 1, 1, 0,
       doc: /* Launch the URL with the appropriate handler application.
               file:// URLs are always opened with the system's default browser, i.e.
               the http:// handler. Return non-nil if the URL has been successfully
               launched.*/)
  (URLstring)
  Lisp_Object URLstring;
{
  check_window_system (NULL);

  CHECK_STRING (URLstring);
  if (NILP (URLstring))
    {
      error ("URL is nil.");
      return Qnil;
    }

  block_input();
  // get default browser



  LSLaunchURLSpec spec;
  OSStatus status;

  if (strncmp("file:/", SDATA(URLstring), 6) == 0)
    {
      /* Build URL to find out what the default handler for http is.
         Without an explicit application reference, the launch function
         (e.g. LSOpenFromURLSpec or ICLaunchURL) will determine the
         default file handler for the file, which is not neccessarily the
         default browser.*/

      FSRef appRef;  // will be discarded
      char* urlStr = "http://www.gnu.org/"; // just a test URL
      CFStringRef inURLCfs = CFStringCreateWithCString(NULL, urlStr,
                                                       kCFStringEncodingASCII);
      CFURLRef inURLRef = CFURLCreateWithString(NULL, inURLCfs, NULL);

      /* Get application for opening html pages */
      status = LSGetApplicationForURL(inURLRef, kLSRolesAll, &appRef,
                                      &spec.appURL);
      CFRelease(inURLRef);
      CFRelease(inURLCfs);
    } else
    {
      spec.appURL = NULL; /* use preferred application */
      status = noErr;
    }
  if (status == noErr)
    {
      /* Open the file / http with the http handler */
      CFStringRef targetUrlCfs =
        CFStringCreateWithCString(NULL, SDATA(URLstring),
                                  kCFStringEncodingASCII);

      /* CFStringRef targetUrlCfsEscaped =
         CFURLCreateStringByAddingPercentEscapes(NULL, targetUrlCfs,
         NULL, NULL,
         kCFStringEncodingUTF8);
         the URL must already be encoded. */
      CFURLRef targetUrlRef =
        CFURLCreateWithString(NULL, targetUrlCfs, NULL);

      if (targetUrlRef)
        {

          if ( (spec.itemURLs =
                CFArrayCreate(NULL, (const void **)&targetUrlRef, 1,
                              &kCFTypeArrayCallBacks)) == NULL)
            {
              return Qnil;
            }
          spec.passThruParams = NULL;
          spec.launchFlags = kLSLaunchDefaults;
          spec.asyncRefCon = NULL;
          status = LSOpenFromURLSpec(&spec, NULL);

          CFRelease(spec.itemURLs);
          CFRelease(targetUrlRef);
        }
      CFRelease(targetUrlCfs);
      /* CFRelease(targetUrlCfsEscaped); */
      unblock_input();

      if (! targetUrlRef)
        {
          error ("Could not produce valid URL from string.");
          return Qnil;
        }
      if (status != noErr)
        {
          error ("Failed to launch default browser. Error %ld", status);
          return Qnil;
        }
    }
  else
    {
      unblock_input();
      error ("Could not determine default browser. Error %ld", status);
      return Qnil;
    }


  return Qt;
}

DEFUN ("ns-open-help-anchor", Fns_open_help_anchor, Sns_open_help_anchor, 1, 2, 0,
       doc: /* Show Apple Help  */)
  (anchor, book)
  Lisp_Object anchor, book;
{
  check_window_system (NULL);
  block_input();
  CHECK_STRING (anchor);

  if (! NILP (book) )
    CHECK_STRING (book);

  [[NSHelpManager sharedHelpManager] openHelpAnchor:[NSString stringWithUTF8String:
								SDATA (anchor)]
					     inBook:(NILP (book) ? nil :
						     [NSString stringWithUTF8String:
								 SDATA (book)])];
  unblock_input();
  return Qnil;
}

DEFUN ("ns-cycle-frame", Fns_cycle_frame, Sns_cycle_frame, 0, 1, "",
       doc: /* Select the next frame in order.
               arg nil means cycle forwards.  */)
  (arg)
  Lisp_Object arg;
{
  if ([NSApp respondsToSelector:@selector(_cycleWindowsReversed:)])
    {
      [NSApp _cycleWindowsReversed:(NILP(arg) ? FALSE : TRUE)];
    }
  return Qnil;
}

DEFUN ("ns-visible-frame-list", Fns_visible_frame_list, Sns_visible_frame_list,
       0, 0, 0,
       doc: /* Return a list of all visible NS frames on the current space.  */)
  (void)
{
  Lisp_Object tail, frame;
  struct frame *f;
  Lisp_Object value;

  value = Qnil;
  for (tail = Vframe_list; CONSP (tail); tail = XCDR (tail))
    {
      frame = XCAR (tail);
      if (!FRAMEP (frame))
	continue;
      f = XFRAME (frame);
      if (FRAME_VISIBLE_P (f)
	  && FRAME_NS_P (f)
	  && (! ([[FRAME_NS_VIEW (f) window] respondsToSelector:@selector(isOnActiveSpace)]) // (NSAppKitVersionNumber
	      || [[FRAME_NS_VIEW (f) window] isOnActiveSpace]))
	value = Fcons (frame, value);
    }
  return value;
}

DEFUN ("ns-frame-is-on-active-space-p", Fns_frame_is_on_active_space_p, Sns_frame_is_on_active_space_p,
       0, 1, 0,
       doc: /* Return non-nil if FRAME is on active space.
               OS X 10.6 only; returns non-nil prior to 10.5 or for non-NS frames.*/)
  (frame)
  Lisp_Object frame;
{
  struct frame *f;
  check_window_system (NULL);
  CHECK_LIVE_FRAME (frame);
  f = XFRAME (frame);
  NSWindow *win = [FRAME_NS_VIEW (f) window];
  if (! ([win respondsToSelector:@selector(isOnActiveSpace)]) // (NSAppKitVersionNumber
      || [win isOnActiveSpace])
    return Qt;
  return Qnil;
}

/* Spelling */

DEFUN ("ns-popup-spellchecker-panel", Fns_popup_spellchecker_panel, Sns_popup_spellchecker_panel,
       0, 0, "",
       doc: /* Pop up the spell checking panel.
               Shows the NS spell checking panel and brings it to the front.*/)
  (void)
{
  id sc;

  check_window_system (NULL);
  sc = [NSSpellChecker sharedSpellChecker];

  block_input();
  [[sc spellingPanel] orderFront: NSApp];

  // Spelling panel should appear with previous content, not empty.
  //  [sc updateSpellingPanelWithMisspelledWord:@""]; // no word, no spelling errors

  // found here: http://trac.webkit.org/changeset/19670
  // // FIXME 4811447: workaround for lack of API
  //  	NSSpellChecker *spellChecker = [NSSpellChecker sharedSpellChecker];
  // does not work
  // if ([sc respondsToSelector:@selector(_updateGrammar)])
  //   [sc performSelector:@selector(_updateGrammar)];
  unblock_input();
  return Qnil;
}

DEFUN ("ns-close-spellchecker-panel", Fns_close_spellchecker_panel, Sns_close_spellchecker_panel,
       0, 0, "",
       doc: /* Close the spell checking panel.*/)
  (void)
{
  id sc;

  check_window_system (NULL);
  sc = [NSSpellChecker sharedSpellChecker];

  block_input();
  [[sc spellingPanel] close];

  unblock_input();
  return Qnil;
}

DEFUN ("ns-spellchecker-panel-visible-p", Fns_spellchecker_panel_visible_p, Sns_spellchecker_panel_visible_p,
       0, 0, "",
       doc: /* Return t if spellchecking panel is visible,
               nil otherwise.*/)
  (void)
{
  id sc;
  BOOL visible;

  check_window_system (NULL);
  sc = [NSSpellChecker sharedSpellChecker];

  block_input();
  visible = [[sc spellingPanel] isVisible];

  unblock_input();
  return visible ? Qt : Qnil;
}


DEFUN ("ns-spellchecker-show-word", Fns_spellchecker_show_word, Sns_spellchecker_show_word,
       1, 1, 0,
       doc: /* Show word WORD in the spellchecking panel.
               Give empty string to delete word.*/)
  (str)
  Lisp_Object str;
{
  id sc;

  CHECK_STRING (str);
  check_window_system (NULL);
  block_input();
  sc = [NSSpellChecker sharedSpellChecker];

  [sc updateSpellingPanelWithMisspelledWord:[NSString stringWithUTF8String: SDATA (str)]]; // no word, no spelling errors

  unblock_input();
  return Qnil;
}


DEFUN ("ns-spellchecker-learn-word", Fns_spellchecker_learn_word, Sns_spellchecker_learn_word,
       1, 1, 0,
       doc: /* Learn word WORD.
               Returns learned word if successful.
               Not available on 10.4.*/)
  (str)
  Lisp_Object str;
{
  CHECK_STRING (str);
  check_window_system (NULL);
  block_input();
  id sc = [NSSpellChecker sharedSpellChecker];

#ifdef NS_IMPL_COCOA
  if ([sc respondsToSelector:@selector(learnWord:)]) // (NSAppKitVersionNumber >= 824.0)
    {

      [sc learnWord:[NSString stringWithUTF8String: SDATA (str)]];
      unblock_input();
      return str;
    }
#endif
  unblock_input();
  return Qnil;
}


DEFUN ("ns-spellchecker-ignore-word", Fns_spellchecker_ignore_word, Sns_spellchecker_ignore_word,
       1, 2, 0,
       doc: /* Ignore word WORD in buffer BUFFER.*/)
  (str, buffer)
  Lisp_Object str, buffer;
{
  id sc;

  CHECK_STRING (str);
  check_window_system (NULL);
  block_input();
  sc = [NSSpellChecker sharedSpellChecker];

  NSInteger tag = 1;
  if (! NILP (buffer))
    {
      tag = sxhash (buffer);
    }

  [sc ignoreWord:[NSString stringWithUTF8String: SDATA (str)] inSpellDocumentWithTag:tag];
  unblock_input();
  return Qnil;
}


DEFUN ("ns-spellchecker-ignored-words", Fns_spellchecker_ignored_words, Sns_spellchecker_ignored_words,
       1, 1, 0,
       doc: /* Return list of words ignored by NSSpellChecker
               for buffer BUFFER */)
  (buffer)
  Lisp_Object buffer;
{
  id sc;

  check_window_system (NULL);
  block_input();
  sc = [NSSpellChecker sharedSpellChecker];

  NSInteger tag = 1;
  if (! NILP (buffer))
    {
      tag = sxhash (buffer);
    }

  Lisp_Object retval = Qnil;
  NSArray *words = [sc ignoredWordsInSpellDocumentWithTag:tag];
  int arrayCount = [words count];
  int i;
  for (i = 0; i < arrayCount; i++) {
    // build Lisp list of strings
    retval = Fcons (build_string ([[words objectAtIndex:i] UTF8String]),
		    retval);
  }
  unblock_input();
  return retval;
}


DEFUN ("ns-spellchecker-check-spelling", Fns_spellchecker_check_spelling, Sns_spellchecker_check_spelling,
       1, 2, 0,
       doc: /* Check spelling of STRING
               Returns the location of the first misspelled word in a
               cons cell of form (beginning . length), or nil if all
               words are spelled as in the dictionary.*/)
  (string, buffer)
  Lisp_Object string, buffer;
{
  id sc;

  CHECK_STRING (string);
  check_window_system (NULL);
  block_input();
  sc = [NSSpellChecker sharedSpellChecker];

  /*  NSRange first_word = nil;   // Invalid initializer!  NSRange is a struct */
  NSInteger tag = 1;
  if (! NILP (buffer) )
    {
      tag = sxhash (buffer);
    }

  /* unfinished -
     if ([sc respondsToSelector:@selector(checkString:range:types:options:inSpellDocumentWithTag:orthography:wordCount:)])
     {
     NSString *nsString = [NSString stringWithUTF8String: SDATA (string)];
     NSArray *spelling_result = [sc
     checkString:nsString
     range:NSMakeRange(0,[nsString size]-1)
     types:NSTextCheckingAllSystemTypes - NSTextCheckingTypeGrammar
     options:nil
     inSpellDocumentWithTag:tag
     orthography:nil // difficult to produce
     wordCount:nil];

     } else */
  // {

  NSRange first_word =  [sc checkSpellingOfString:[NSString stringWithUTF8String: SDATA (string)] startingAt:((NSInteger) 0)
                                         language:nil wrap:NO inSpellDocumentWithTag:tag wordCount:nil];

  // }
  unblock_input();
  if (first_word.location == NSNotFound || (int) first_word.location < 0)
    return Qnil;
  else
    return Fcons (make_fixnum (first_word.location), make_fixnum (first_word.length));
}


DEFUN ("ns-spellchecker-check-grammar", Fns_spellchecker_check_grammar, Sns_spellchecker_check_grammar,
       1, 2, 0,
       doc: /* Check spelling of SENTENCE.
               BUFFER, if given, idenitifies the document containing list
               of ignored grammatical constructions. */)
  (sentence, buffer)
  Lisp_Object sentence, buffer;
{
  id sc;

  CHECK_STRING (sentence);
  check_window_system (NULL);
  block_input();
  sc = [NSSpellChecker sharedSpellChecker];

  NSInteger tag = 1;
  if (! NILP (buffer) )
    {
      tag = sxhash (buffer);
    }

  NSArray *errdetails;

  /* to do: use long version */
  NSRange first_word = [sc checkGrammarOfString: [NSString stringWithUTF8String: SDATA (sentence)] startingAt:((NSInteger) 0)
				       language:nil wrap:NO inSpellDocumentWithTag:tag details:&errdetails];

  unblock_input();
  if (first_word.length == 0) // Is this how "no location" is indicated?
    return Qnil;
  else
    return Fcons (make_fixnum ((int) first_word.location), make_fixnum ((int) first_word.length));
}


DEFUN ("ns-spellchecker-get-suggestions", Fns_spellchecker_get_suggestions, Sns_spellchecker_get_suggestions,
       1, 1, 0,
       doc: /* Get suggestions for WORD.
               If word contains all capital letters, or its first
               letter is capitalized, the suggested words are
               capitalized in the same way. */)
  (word)
  Lisp_Object word;
{
  id sc;

  CHECK_STRING (word);
  check_window_system (NULL);
  block_input();
  sc = [NSSpellChecker sharedSpellChecker];

  Lisp_Object retval = Qnil;
  NSString *the_word = [NSString stringWithUTF8String: SDATA (word)];
  NSArray *guesses = [sc guessesForWordRange:NSMakeRange(0, [the_word length]) inString:the_word language:[sc language] inSpellDocumentWithTag:0];
  int arrayCount = [guesses count];
  int i = arrayCount;
  while (--i >= 0)
    retval = Fcons (build_string ([[guesses objectAtIndex:i] UTF8String]),
		    retval);
  unblock_input();
  return retval;
}


DEFUN ("ns-spellchecker-list-languages", Fns_spellchecker_list_languages, Sns_spellchecker_list_languages,
       0, 0, 0,
       doc: /* Get all available spell-checking languages.
               Returns nil if not successful.*/)
  (void)
{
  id sc;
  Lisp_Object retval = Qnil;

  check_window_system (NULL);
  block_input();
  sc = [NSSpellChecker sharedSpellChecker];

#ifdef NS_IMPL_COCOA
  if ([sc respondsToSelector:@selector(availableLanguages)]) // (NSAppKitVersionNumber >= 824.0)
    {
      NSArray *langs = [sc availableLanguages];
      int arrayCount = [langs count];
      int i;
      for (i = 0; i < arrayCount; i++) {
	// build Lisp list of strings
	retval = Fcons (build_string ([[langs objectAtIndex:i] UTF8String]),
			retval);
      }
    }
#endif
  unblock_input();
  return retval;
}


DEFUN ("ns-spellchecker-current-language", Fns_spellchecker_current_language, Sns_spellchecker_current_language,
       0, 0, 0,
       doc: /* Get the current spell-checking language.*/)
  (void)
{
  id sc;

  check_window_system (NULL);
  block_input();
  sc = [NSSpellChecker sharedSpellChecker];

  Lisp_Object retval = Qnil;
  NSString *lang = [sc language];
  retval = build_string ([lang UTF8String]);

  unblock_input();
  return retval;
}


DEFUN ("ns-spellchecker-set-language", Fns_spellchecker_set_language, Sns_spellchecker_set_language,
       1, 1, 0,
       doc: /* Set spell-checking language.
               LANGUAGE must be one of the languages returned by
               `ns-spellchecker-list-langauges'.*/)
  (language)
  Lisp_Object language;
{
  id sc;

  CHECK_STRING (language);
  check_window_system (NULL);
  block_input();
  sc = [NSSpellChecker sharedSpellChecker];

  [sc setLanguage: [NSString stringWithUTF8String: SDATA (language)]];
  unblock_input();
  return Qnil;
}


static NSString *
ns_filename_from_panel (NSSavePanel *panel)
{
#ifdef NS_IMPL_COCOA
  NSURL *url = [panel URL];
  NSString *str = [url path];
  return str;
#else
  return [panel filename];
#endif
}

Lisp_Object
ns_color_to_lisp (NSColor *col)
/* --------------------------------------------------------------------------
   Convert a color to a lisp string with the RGB equivalent
   -------------------------------------------------------------------------- */
{
  EmacsCGFloat red, green, blue, alpha, gray;
  char buf[1024];
  const char *str;
  NSTRACE ("ns_color_to_lisp");

  block_input ();
  if ([[col colorSpaceName] isEqualToString: NSNamedColorSpace])

    if ((str =[[col colorNameComponent] UTF8String]))
      {
        unblock_input ();
                                                                                            return build_string ((char *)str);
      }

  [[col colorUsingDefaultColorSpace]
        getRed: &red green: &green blue: &blue alpha: &alpha];
  if (red == green && red == blue)
    {
      unblock_input ();
      [[col colorUsingColorSpaceName: NSCalibratedWhiteColorSpace]
            getWhite: &gray alpha: &alpha];
      snprintf (buf, sizeof (buf), "#%2.2lx%2.2lx%2.2lx",
		lrint (gray * 0xff), lrint (gray * 0xff), lrint (gray * 0xff));
      return build_string (buf);
    }

  snprintf (buf, sizeof (buf), "#%2.2lx%2.2lx%2.2lx",
            lrint (red*0xff), lrint (green*0xff), lrint (blue*0xff));

  unblock_input ();
  return build_string (buf);
}

static NSString *
ns_directory_from_panel (NSSavePanel *panel)
{
#ifdef NS_IMPL_COCOA
  NSURL *url = [panel directoryURL];
  NSString *str = [url path];
  return str;
#else
  return [panel directory];
#endif
}

/*
  Handle arrow/function/control keys and copy/paste/cut in file dialogs.
  Return YES if handled, NO if not.
*/
static BOOL
handlePanelKeys (NSSavePanel *panel, NSEvent *theEvent)
{
  NSString *s;
  int i;
  BOOL ret = NO;

  return NO;
  if ([theEvent type] != NSKeyDown) return NO;
  s = [theEvent characters];

  for (i = 0; i < [s length]; ++i)
    {
      int ch = (int) [s characterAtIndex: i];
      switch (ch)
        {
        case NSHomeFunctionKey:
        case NSDownArrowFunctionKey:
        case NSUpArrowFunctionKey:
        case NSLeftArrowFunctionKey:
        case NSRightArrowFunctionKey:
        case NSPageUpFunctionKey:
        case NSPageDownFunctionKey:
        case NSEndFunctionKey:
          /* Don't send command modified keys, as those are handled in the
             performKeyEquivalent method of the super class.
          */
          if (! ([theEvent modifierFlags] & NSCommandKeyMask))
            {
              [panel sendEvent: theEvent];
              ret = YES;
            }
          break;
          /* As we don't have the standard key commands for
             copy/paste/cut/select-all in our edit menu, we must handle
             them here.  TODO: handle Emacs key bindings for copy/cut/select-all
             here, paste works, because we have that in our Edit menu.
             I.e. refactor out code in nsterm.m, keyDown: to figure out the
             correct modifier.
          */
        case 'x': // Cut
        case 'c': // Copy
        case 'v': // Paste
        case 'a': // Select all
          if ([theEvent modifierFlags] & NSCommandKeyMask)
            {
              [NSApp sendAction:
                       (ch == 'x'
                        ? @selector(cut:)
                        : (ch == 'c'
                           ? @selector(copy:)
                           : (ch == 'v'
                              ? @selector(paste:)
                              : @selector(selectAll:))))
                             to:nil from:panel];
              ret = YES;
            }
        default:
          // Send all control keys, as the text field supports C-a, C-f, C-e
          // C-b and more.
          if ([theEvent modifierFlags] & NSControlKeyMask)
            {
              [panel sendEvent: theEvent];
              ret = YES;
            }
          break;
        }
    }


  return ret;
}

@implementation EmacsSavePanel

- (void)becomeKeyWindow
{
  [super becomeKeyWindow];
  [NSApp setMainMenu: [panelMenu retain]];

}

- (NSString *) getFilename
{
  return ns_filename_from_panel (self);
}

- (NSString *) getDirectory
{
  return ns_directory_from_panel (self);
}

- (BOOL)performKeyEquivalent:(NSEvent *)theEvent
{
  BOOL ret = handlePanelKeys (self, theEvent);
  if (! ret)
    ret = [super performKeyEquivalent:theEvent];
  return ret;
}
@end


@implementation EmacsOpenPanel
- (BOOL)performKeyEquivalent:(NSEvent *)theEvent
{
  // NSOpenPanel inherits NSSavePanel, so passing self is OK.
  BOOL ret = handlePanelKeys (self, theEvent);
  if (! ret)
    ret = [super performKeyEquivalent:theEvent];
  return ret;
}
@end


/* ==========================================================================

   Lisp interface declaration

   ========================================================================== */

void
syms_of_nsaquamacs (void)
{

  DEFVAR_LISP ("ns-spelling-text", ns_spelling_text,
               "The substitute text corresponding to the ns-spelling-change event.");
  ns_spelling_text =Qnil;

  DEFVAR_LISP ("ns-input-background-color", ns_input_background_color,
               "The background color specified in the last NS event.");
  ns_input_background_color =Qnil;

  DEFVAR_LISP ("ns-save-panel-file", ns_save_panel_file,
               "The file in the NS save panel.");
  ns_save_panel_file =Qnil;

  DEFVAR_LISP ("ns-save-panel-buffer", ns_save_panel_buffer,
               "The buffer related to the NS save panel.");
  ns_save_panel_buffer =Qnil;

  DEFVAR_LISP ("ns-alternate-meta-special-codes", ns_alternate_meta_special_codes,
               "List of key codes (numbers) identifying special keys.\n\
For these special keys, the Option modifier will be interpreted as \n\
Meta if `ns_alternate_modifier' and `ns_right_alternate_modifier' are\n\
configured such that the other keys with the Option modifier would\n\
be interpreted by the system (`none' or nil), and similiarly for the\n\
control modifier.\n\
\n\
If `ns_alternate_modifier' and `ns_right_alternate_modifier' are\n\
set to a specific Emacs modifier, this will be respected.");
  ns_alternate_meta_special_codes = Qnil;

  DEFVAR_LISP ("ns-session-restore-request", ns_session_restore_request,
               "Non-nil if a sesion restore request was received.\n\
This system-level event is usually received while the\n\
application launches.");
  ns_session_restore_request = Qnil;

  DEFVAR_LISP ("ns-emulate-three-button-mouse", ns_emulate_three_button_mouse,
               "Non-nil (the default) means to use mouse button emulation with modifiers.\n\
Ccontrol and command keys are used to emulate right and middle mouse\n\
buttons on a one-button mouse.");
  ns_emulate_three_button_mouse = Qt;

  defsubr (&Sns_launch_url_with_default_browser);
  defsubr (&Sns_send_odb_notification);
  defsubr (&Sns_application_hidden_p);
  defsubr (&Sns_os_version);
  defsubr (&Sns_cycle_frame);
  defsubr (&Sns_visible_frame_list);
  defsubr (&Sns_frame_is_on_active_space_p);
  defsubr (&Sns_popup_spellchecker_panel);
  defsubr (&Sns_close_spellchecker_panel);
  defsubr (&Sns_spellchecker_panel_visible_p);
  defsubr (&Sns_spellchecker_learn_word);
  defsubr (&Sns_spellchecker_ignore_word);
  defsubr (&Sns_spellchecker_ignored_words);
  defsubr (&Sns_spellchecker_show_word);
  defsubr (&Sns_spellchecker_check_spelling);
  defsubr (&Sns_spellchecker_check_grammar);
  defsubr (&Sns_spellchecker_get_suggestions);
  defsubr (&Sns_spellchecker_list_languages);
  defsubr (&Sns_spellchecker_current_language);
  defsubr (&Sns_spellchecker_set_language);
  defsubr (&Sns_popup_print_panel);
  defsubr (&Sns_popup_page_setup_panel);
  defsubr (&Sns_popup_save_panel);
  defsubr (&Saquamacs_render_to_pdf);
  defsubr (&Saquamacs_html_to_rtf);
  defsubr (&Sns_open_help_anchor);
}
