#!/usr/bin/env python

from Category import *
from ValueMatrix import *

import sys
import cgi

# Some error classes for exception raising.
class FeatureMatrixError: pass
class NonLeafProduct(FeatureMatrixError): pass
class NonLeafFeature(FeatureMatrixError): pass

class FeatureMatrix:
    def __init__(self, title):
	self.title = title
	self.products = Category()
	self.features = Category()
	self.values = ValueMatrix()

    def get_products(self):
	return self.products;
    
    def get_features(self):
	return self.features
    
    def add(self, product, feature, value):
	if product.get_subcategories():
	    print >> sys.stderr, sys.argv[0] + ": Product '%s' is not a leaf." % product.get_name()
	    raise NonLeafFeature
	if feature.get_subcategories():
	    print >> sys.stderr, sys.argv[0] + ": Feature '%s' is not a leaf." % feature.get_name()
	    raise NonLeafProduct
	self.values.add(product, feature, value)

    def html_print(self, style=''):
	print '<html>'
	print style
	print '<body><p>%s</p><table>\n' % cgi.escape(self.title)
	# Sub one in the following because we ignore the (useless to us) first level in the product/feature matrix.
	feature_depth = self.features.max_depth() - 1
	product_depth = self.products.max_depth() - 1
	product_list = self.products.leaves()

	# Print the product hierarchy
	print self.products.as_cols(self.products.get_subcategories(), True, feature_depth, product_depth, 1)
	
	# Print the feature hierarchy and feature values
	for feature in self.features.get_subcategories():
	    print feature.as_rows(False, product_list, self.values, feature_depth, 1)
	
	print "</table></body></html>"

if __name__ == '__main__':
    matrix = FeatureMatrix('New feature matrix')

    products = matrix.get_products()
    emacs = products.add_node('Emacs')
    vi = products.add_node('Vi')

    features = matrix.get_features()    
    lisp = features.add_node('Lisp')
    interpreted = features.add_node('Interpreted')
    speed = features.add_node('Speed')
    lightning = speed.add_node('Lightning')
    slow = speed.add_node('Slow')
    lightning.add_node('Greased')
    lightning.add_node('Regular')

    matrix.add_product_feature(emacs, slow, 'YES')
    
    matrix.html_print()
