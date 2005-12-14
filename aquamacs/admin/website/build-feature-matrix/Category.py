#!/usr/bin/env python

from ValueMatrix import *
import types

class Category:
    def __init__(self, name = '', path = None):
	self.name = name
	self.subcategories = []
	if path:
	    self.path = path
	else:
	    self.path = name

    def get_name(self):
	return self.name

    def get_path(self):
	return self.path

    def get_subcategories(self):
	return self.subcategories

    def n_subcategories(self):
	return len(self.subcategories)

    def max_depth(self):
	if self.subcategories:
	    max = -1
	    for sub in self.subcategories:
		this = sub.max_depth()
		if this > max:
		    max = this
	    return max + 1

	return 1

    def leaves(self):
	if self.subcategories:
	    l = []
	    for sub in self.subcategories:
		l.extend(sub.leaves())
	    return l

	return [ self ]

    def n_leaves(self):
	if self.subcategories:
	    sum = 0
	    for sub in self.subcategories:
		sum += sub.n_leaves()
	    return sum

	return 1

    def add_subcategory(self, name):
	new = Category(name, self.get_path() + ' / ' + name )
	self.subcategories.append(new)
	return new

    def as_rows(self, tr_started, products, values, feature_depth, depth):
	rep = ''
	if not tr_started:
	    rep += '<tr>'
	    tr_started = True
	rep += '<td class="feature%d"' % depth
	if self.subcategories:
	    rep += ' valign="top" rowspan="%d"' % self.n_leaves()
	else:
	    if feature_depth - depth > 0:
		rep += ' colspan="%d"' % (feature_depth - depth + 1)
	rep += '>%s</td>' % cgi.escape(self.get_name())

	if self.subcategories:
	    subfirst = True
	    for sub in self.subcategories:
		rep += sub.as_rows(subfirst, products, values, feature_depth, depth + 1)
		subfirst = False
	else:
	    for product in products:
		rep += '<td class="value" valign="center">%s</td>' % values.get(product, self)
	    if tr_started:
		rep += '</tr>'
	    
	return rep + '\n'

    def as_cols(self, categories, first_row, feature_depth, product_depth, depth):
	if first_row:
	    rep = '<tr><td class="topleft" valign="top" colspan="%d" rowspan="%d">Feature</td>' % (feature_depth, product_depth)
	else:
	    rep = '<tr>'

	subcategories = []
	for category in categories:
	    rep += '<td valign="top" class="product%d"' % depth
	    n_leaves = category.n_leaves()
	    if n_leaves > 1:
		rep += ' colspan="%d"' % n_leaves
	    # If there are no subcategories and we're not at the bottom, add a rowspan
	    if not category.subcategories and product_depth - depth > 0:
		rep += ' rowspan="%d"' % (product_depth - depth + 1)
	    rep += '>%s</td>' % cgi.escape(category.get_name())
	    if category.subcategories:
		subcategories.extend(category.subcategories)

	# The self in the following is really irrelevant, perhaps a sign of bad OO design.
	rep += '</tr>'
	if subcategories:
	    rep += self.as_cols(subcategories, False, feature_depth, product_depth, depth + 1)

	return rep + '\n'
	
    def add_node(self, names, sep = None):
	if not names:
	    return self
	 
	if type(names) == types.StringType:
	    if sep:
		for name in names.split(sep):
		    return self.add_node(name.strip())
	    else:
		return self.add_node([ names ])
	else:
	    # We have a list of names.
	    name = names[0]
	    if len(names) > 1:
		rest = names[1:]
	    else:
		rest = None
	    for sub in self.subcategories:
		if name == sub.name:
		    return sub.add_node(rest)
	    
	    new = self. add_subcategory(name)
	    return new.add_node(rest)
