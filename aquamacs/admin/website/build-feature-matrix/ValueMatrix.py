#!/usr/bin/env python

import cgi
import os.path

class Value:
    def __init__(self, value):
	self.value = value

    def __str__(self):
	return self.value

class QuotedValue(Value):
    def __str__(self):
	return cgi.escape(self.value)

class ImageValue(Value):
    def __init__(self, image, count = 1):
	if not os.path.isfile(image):
	    raise OSError
	self.value = ('<img valign="top" src="%s">' % image) * count

class NbspValue(Value):
    def __init__(self):
	self.value = '&nbsp;'

class ValueMatrix:
    def __init__(self, default=NbspValue()):
	self.default = default
	self.values = {}
    
    def add(self, product, feature, value):
	if not self.values.has_key(product):
	    self.values[product] = {}
	self.values[product][feature] = value
	
    def get(self, product, feature):
	if self.values.has_key(product):
	    p = self.values[product]
	    if p.has_key(feature):
		return p[feature].__str__()
	return self.default.__str__()
	

if __name__ == '__main__':
    v = Value('first')
    print v
    v = QuotedValue('second')
    print v
    v = QuotedValue('<sec&ond>')
    print v
    v = NbspValue()
    print v

    v = ValueMatrix()
    v.add(1, 2, QuotedValue('terry'))
    print v.get(1, 2)
    print v.get(4, 2)
