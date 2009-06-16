#!/usr/bin/env python

from FeatureMatrix import *
from ValueMatrix import *

# Minus sign images (= absence of feature)
minus1 = ImageValue('images/minus.png', 1)
minus2 = ImageValue('images/minus.png', 2)
minus3 = ImageValue('images/minus.png', 3)
minus = minus1

# Checkmark images (= presence of feature)
plus1 = ImageValue('images/plus.jpg', 1)
plus2 = ImageValue('images/plus.jpg', 2)
plus3 = ImageValue('images/plus.jpg', 3)
plus = plus1

# Question mark image (presence of feature uncertain)
quest = ImageValue('images/quest.png', 1)

fonts = 'Verdana, Arial, Helvetica, sans-serif'
font_size = 14
small_font_size = 10

style = '<style type="text/css">\n' + \
    'p {font-family: %s; font-size: %dpx}\n' % (fonts, font_size) + \
    'td {font-family: %s; font-size: %dpx; border: 1px solid}\n'  % (fonts, font_size) + \
    'body { font-family: %s; font-size: %dpx}\n' % (fonts, font_size) + \
    'td.product1 { background: #66bbff; }\n' + \
    'td.product2 { background: #80bbff; font-size: %dpx; }\n' % (small_font_size) + \
    'td.feature1 { background: #66bbff; }\n' + \
    'td.feature2 { background: #80bbff; font-size: %dpx; }\n' % (small_font_size)+ \
    'td.topleft { background: #ccccdd; }\n' + \
    'td.value { background: #eeeeee; font-size: %dpx }\n' % (small_font_size) + \
    'table { border-collapse: collapse; }\n' + \
    '</style>\n'

matrix = FeatureMatrix('Aquamacs feature matrix')

# Add some products (these will appear in the order added).
products   = matrix.get_products()
aquamacs = products.add_node('Aquamacs Emacs 0.9.7')
emacs       = products.add_node('GNU Emacs (CVS 10/2005)')
# Example of adding product subcategories. If you do this though, you'll have
# to change things below, as 'emacs' will no longer be a product hierarchy
# leaf node.
# f = emacs.add_node('19')
# x = emacs.add_node('20')
# f = x.add_node('1')
# f = x.add_node('2')
bbedit       = products.add_node('BBEdit 8.0')
smultron  = products.add_node('Smultron')


# Add some features (these will appear in the order added).
features = matrix.get_features()

f = features.add_node('Copy & Paste')
subf = f.add_node('X11/Unix-like')
matrix.add(emacs, subf, plus)
matrix.add(smultron, subf, minus)

subf = f.add_node('Mac-like')
matrix.add(aquamacs, subf, plus)
matrix.add(bbedit, subf, plus)
matrix.add(smultron, subf, plus)

f = features.add_node('Syntax Coloring')

subf = f.add_node('Major languages like C and XML')
matrix.add(aquamacs, subf, plus)
matrix.add(emacs, subf, plus)
matrix.add(bbedit, subf, plus)
matrix.add(smultron, subf, plus)

subf = f.add_node('Exotic languages like Prolog')
matrix.add(aquamacs, subf, plus)
matrix.add(smultron, subf, plus)

f = features.add_node('Printing')

subf = f.add_node('WYSIWYG')
matrix.add(aquamacs, subf, plus)
matrix.add(emacs, subf, plus)
matrix.add(bbedit, subf, plus)
matrix.add(smultron, subf, plus)

subf = f.add_node('Postscript')
matrix.add(aquamacs, subf, plus)
matrix.add(emacs, subf, plus)
matrix.add(bbedit, subf, minus)
matrix.add(smultron, subf, minus)

f = features.add_node('Code folding')
matrix.add(aquamacs, f, '(outline mode)')
matrix.add(emacs, f, '(outline mode)')
matrix.add(bbedit, f, minus)
matrix.add(smultron, f, minus)

f = features.add_node('Free use of all fonts')
matrix.add(aquamacs, f, plus)
matrix.add(emacs, f, minus)
matrix.add(bbedit, f, plus)
matrix.add(smultron, f, plus)

f = features.add_node('HTML Support')
matrix.add(aquamacs, f, 'good')
matrix.add(emacs, f, 'ok')
matrix.add(bbedit, f, 'great')
matrix.add(smultron, f, plus)

f = features.add_node('LaTeX Support')
matrix.add(aquamacs, f, 'very good')
matrix.add(emacs, f, 'decent')
matrix.add(bbedit, f, 'basic')
matrix.add(smultron, f, 'basic')

f = features.add_node('Reformatting / Indenting (syntax based)')
matrix.add(aquamacs, f, plus2)
matrix.add(emacs, f, plus2)
matrix.add(bbedit, f, minus)
matrix.add(smultron, f, minus)

f = features.add_node('Auto-complete phrases ("Code Sense")')
matrix.add(aquamacs, f, plus)
matrix.add(emacs, f, plus)
matrix.add(bbedit, f, quest)
matrix.add(smultron, f, plus)

f = features.add_node('Color Themes')

subf = f.add_node('Supported')
matrix.add(aquamacs, subf, plus)
matrix.add(emacs, subf, minus)
matrix.add(bbedit, subf, plus)
matrix.add(smultron, subf, minus)

subf = f.add_node('predefined sets')
matrix.add(aquamacs, subf, plus)
matrix.add(emacs, subf, minus)
matrix.add(bbedit, subf, minus)
matrix.add(smultron, subf, minus)

f = features.add_node('Exports to')

subf = f.add_node('PDF')
matrix.add(aquamacs, subf, plus)
matrix.add(emacs, subf, minus)
matrix.add(bbedit, subf, 'via Print')
matrix.add(smultron, subf, plus)

subf = f.add_node('HTML')
matrix.add(aquamacs, subf, plus)
matrix.add(emacs, subf, minus)
matrix.add(bbedit, subf, minus)
matrix.add(smultron, subf, minus)

f = features.add_node('Unix Shell Support')
matrix.add(aquamacs, f, plus2)
matrix.add(emacs, f, plus2)
matrix.add(bbedit, f, minus) # Need to add (Shell Workspaces ++) to this
matrix.add(smultron, f, minus)

f = features.add_node('Tool Bar')

subf = f.add_node('Supported')
matrix.add(aquamacs, subf, plus)
matrix.add(emacs, subf, plus)
matrix.add(bbedit, subf, plus)
matrix.add(smultron, subf, plus)

subf = f.add_node('Individually switchable')
matrix.add(aquamacs, subf, plus)
matrix.add(emacs, subf, minus)
matrix.add(bbedit, subf, plus)
matrix.add(smultron, subf, plus)

subf = f.add_node('Mode/language-specific')
matrix.add(aquamacs, subf, plus)
matrix.add(emacs, subf, plus)
matrix.add(bbedit, subf, minus)
matrix.add(smultron, subf, minus)

f = features.add_node('Help / Documentation')
matrix.add(aquamacs, f, 'Apple Help, PDF, info')
matrix.add(emacs, f, 'only info, PDF online')
matrix.add(bbedit, f, 'Apple Help (some), PDF')
matrix.add(smultron, f, 'Apple Help (basic)')

f = features.add_node('Compare and Merge')
matrix.add(aquamacs, f, plus)
matrix.add(emacs, f, plus)
matrix.add(bbedit, f, plus)
matrix.add(smultron, f, minus)

f = features.add_node('Search & Replace: Regular Expressions')
matrix.add(aquamacs, f, plus)
matrix.add(emacs, f, plus)
matrix.add(bbedit, f, plus)
matrix.add(smultron, f, plus)

f = features.add_node('Scriptable? Macro-Language?')
matrix.add(aquamacs, f, 'fully (elisp)')
matrix.add(emacs, f, 'fully (elisp)')
matrix.add(bbedit, f, 'partially (AppleScript)')
matrix.add(smultron, f, minus)

f = features.add_node('Version Control')
matrix.add(aquamacs, f, 'CVS, SVN')
matrix.add(emacs, f, 'CVS, SVN')
matrix.add(bbedit, f, 'CVS, SVN')
matrix.add(smultron, f, minus)

f = features.add_node('Multi-Language Input Methods')
matrix.add(aquamacs, f, 'native, Emacs')
matrix.add(emacs, f, 'native, Emacs')
matrix.add(bbedit, f, 'native')
matrix.add(smultron, f, minus)

f = features.add_node('Multiple Buffers per Window')

subf = f.add_node('Supported')
matrix.add(aquamacs, subf, plus)
matrix.add(emacs, subf, plus)
matrix.add(bbedit, subf, plus)
matrix.add(smultron, subf, plus)

subf = f.add_node('Tabbed Editing')
matrix.add(aquamacs, subf,  minus)
matrix.add(emacs, subf, minus)
matrix.add(bbedit, subf, plus)
matrix.add(smultron, subf, plus)

subf = f.add_node('Window per file')
matrix.add(aquamacs, subf, plus)
matrix.add(emacs, subf, minus)
matrix.add(bbedit, subf, plus)
matrix.add(smultron, subf, plus)

f = features.add_node('Saves cursor position in file persistently')
matrix.add(aquamacs, f, plus)
matrix.add(emacs, f, plus)
matrix.add(bbedit, f, minus)
matrix.add(smultron, f, minus)

f = features.add_node('Customizability')
matrix.add(aquamacs, f, 'extensive, often hard')
matrix.add(emacs, f, 'extensive, almost always hard')
matrix.add(bbedit, f, 'some, easy')
matrix.add(smultron, f, 'basic, easy')

f = features.add_node('Cross-Platform')
matrix.add(aquamacs, f, 'subset')
matrix.add(emacs, f, 'fully')
matrix.add(bbedit, f, minus)
matrix.add(smultron, f, minus)

f = features.add_node('Spell-Checking')
matrix.add(aquamacs, f, 'OS X native, ispell, aspell')
matrix.add(emacs, f, 'ispell, aspell (extra install)')
matrix.add(bbedit, f, 'OS X native')
matrix.add(smultron, f, 'OS X native')

f = features.add_node('Overall')

subf = f.add_node('Features')
matrix.add(aquamacs, subf, plus3)
matrix.add(emacs, subf, plus2)
matrix.add(bbedit, subf, plus)
matrix.add(smultron, subf, plus)

subf = f.add_node('Ease of Use for Novices')
matrix.add(aquamacs, subf, plus2)
matrix.add(emacs, subf, minus)
matrix.add(bbedit, subf, plus3)
matrix.add(smultron, subf, plus3)

subf = f.add_node('Extensibility')
matrix.add(aquamacs, subf, plus3)
matrix.add(emacs, subf, plus3)
matrix.add(bbedit, subf, plus)
matrix.add(smultron, subf, minus)

subf = f.add_node('Compliance with Mac UI')
matrix.add(aquamacs, subf, plus)
matrix.add(emacs, subf, minus2)
matrix.add(bbedit, subf, plus2)
matrix.add(smultron, subf, plus2)

subf = f.add_node('Compliance with Emacs UI')
matrix.add(aquamacs, subf, plus2)
matrix.add(emacs, subf, plus3)
matrix.add(bbedit, subf, 'N/A')
matrix.add(smultron, subf, 'N/A')

f = features.add_node('Built-in Shrink :-)')
matrix.add(aquamacs, f, plus)
matrix.add(emacs, f, plus)
matrix.add(bbedit, f, minus)
matrix.add(smultron, f, minus)

f = features.add_node('Price')
matrix.add(aquamacs, f, 'free, donation USD$/&euro;20')
matrix.add(emacs, f, 'free')
matrix.add(bbedit, f, 'USD$199')
matrix.add(smultron, f, 'free')

# I use 'GPL' below, not 'free, GPL' as a comma above feature values implies 'and',
# whereas emacs is not 'free' as in beer etc...

f = features.add_node('License / Source')
matrix.add(aquamacs, f, 'GPL')
matrix.add(emacs, f, 'GPL')
matrix.add(bbedit, f, 'closed source')
matrix.add(smultron, f, 'GPL')

matrix.html_print(style)
