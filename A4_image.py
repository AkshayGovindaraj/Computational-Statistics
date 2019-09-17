#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Mar  3 10:12:41 2019

@author: akshaygovindaraj
"""

import matplotlib.pyplot as plt
import matplotlib.image as img
import numpy as np

filename = 'yalefaces/resize.gif'
image = img.imread(filename)
imgt = np.reshape(image[:,:,0], 1200)

filename = 'yalefaces/resize1.gif'
image = img.imread(filename)
imgt1 = np.reshape(image[:,:,0], 1200)

filename = 'yalefaces/resize2.gif'
image = img.imread(filename)
imgt2 = np.reshape(image[:,:,0], 1200)

filename = 'yalefaces/resize3.gif'
image = img.imread(filename)
imgt3 = np.reshape(image[:,:,0], 1200)

filename = 'yalefaces/resize4.gif'
image = img.imread(filename)
imgt4 = np.reshape(image[:,:,0], 1200)

filename = 'yalefaces/resize6.gif'
image = img.imread(filename)
imgt6 = np.reshape(image[:,:,0], 1200)

filename = 'yalefaces/resize7.gif'
image = img.imread(filename)
imgt7 = np.reshape(image[:,:,0], 1200)

filename = 'yalefaces/resize8.gif'
image = img.imread(filename)
imgt8 = np.reshape(image[:,:,0], 1200)

filename = 'yalefaces/resize9.gif'
image = img.imread(filename)
imgt9 = np.reshape(image[:,:,0], 1200)

ab = np.matrix([imgt.tolist(), imgt1.tolist(), imgt2.tolist(), imgt3.tolist(), imgt4.tolist(), imgt6.tolist(), imgt7.tolist(), imgt8.tolist(), imgt9.tolist()])
u, s, vh = np.linalg.svd(ab, full_matrices=True)
plt.imshow(np.reshape(vh[:,1], (30,40)), cmap="Greys")

#For subject 1
filename = 'yalefaces/resize15.gif'
image = img.imread(filename)
imgt15 = np.reshape(image[:,:,0], 1200)

filename = 'yalefaces/resize11.gif'
image = img.imread(filename)
imgt11 = np.reshape(image[:,:,0], 1200)

filename = 'yalefaces/resize12.gif'
image = img.imread(filename)
imgt12 = np.reshape(image[:,:,0], 1200)

filename = 'yalefaces/resize13.gif'
image = img.imread(filename)
imgt13 = np.reshape(image[:,:,0], 1200)

filename = 'yalefaces/resize14.gif'
image = img.imread(filename)
imgt14 = np.reshape(image[:,:,0], 1200)

filename = 'yalefaces/resize16.gif'
image = img.imread(filename)
imgt16 = np.reshape(image[:,:,0], 1200)

filename = 'yalefaces/resize17.gif'
image = img.imread(filename)
imgt17 = np.reshape(image[:,:,0], 1200)

filename = 'yalefaces/resize18.gif'
image = img.imread(filename)
imgt18 = np.reshape(image[:,:,0], 1200)

filename = 'yalefaces/resize19.gif'
image = img.imread(filename)
imgt19 = np.reshape(image[:,:,0], 1200)

ab1 = np.matrix([imgt15.tolist(), imgt11.tolist(), imgt12.tolist(), imgt13.tolist(), imgt14.tolist(), imgt16.tolist(), imgt17.tolist(), imgt18.tolist(), imgt19.tolist()])
u1, s1, vh1 = np.linalg.svd(ab1, full_matrices=True)
plt.imshow(np.reshape(vh1[:,1], (30,40)), cmap="Greys")

#Dot product with first eigenvector
filename = 'yalefaces/resize10.gif'
image = img.imread(filename)
test_vector = np.reshape(image[:,:,0], 1200)

c14 = np.dot(test_vector, vh[:,1])
c1 = np.dot(test_vector, vh1[:,1])