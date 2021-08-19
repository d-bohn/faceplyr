# Author: Daniel Albohn (c) 2017
# Adapted from: https://github.com/pathak22/pyflow/blob/master/demo.py
# Original Author: Deepak Pathak (c) 2016

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
# from __future__ import unicode_literals
import numpy as np
from PIL import Image
import time
import argparse
import pandas as pd
import pyflow

parser = argparse.ArgumentParser(
    description='python wrapper for Coarse2Fine Optical Flow')
parser.add_argument(
    '-r', dest='reference', action='store', required=True)
parser.add_argument(
    '-t', dest='target', action='store', required=True)
parser.add_argument(
    '-o', dest='output', action='store', required=True)
parser.add_argument(
        '-s', dest='save', action='store', required=True)
parser.add_argument(
    '-v', dest='viz', action='store_true')
args = parser.parse_args()

im1 = np.array(Image.open(args.reference))
if len(im1.shape) != 3 :
    im1 = np.expand_dims(im1, axis=2)

im2 = np.array(Image.open(args.target))
if len(im2.shape) != 3 :
    im2 = np.expand_dims(im2, axis=2)

im1 = im1.astype(float) / 255.
im2 = im2.astype(float) / 255.

# Flow Options:
alpha = 0.012
ratio = 0.75
minWidth = 20
nOuterFPIterations = 7
nInnerFPIterations = 1
nSORIterations = 30
colType = 1  # 0 or default:RGB, 1:GRAY (but pass gray image with shape (h,w,1))

s = time.time()
u, v, im2W = pyflow.coarse2fine_flow(
    im1, im2, alpha, ratio, minWidth, nOuterFPIterations, nInnerFPIterations,
    nSORIterations, colType)
e = time.time()
#print('Time Taken: %.2f seconds for image of size (%d, %d, %d)' % (
#    e - s, im1.shape[0], im1.shape[1], im1.shape[2]))
flow = np.concatenate((u[..., None], v[..., None]), axis=2)
np.save(args.output+'/'+args.save+'.npy', flow)
#df = pd.DataFrame(flow)
#df.to_csv(args.output+'/'+args.save+'.csv')

#print im1.shape

if args.viz:
    import cv2
    hsv = np.zeros(im1.shape, dtype=np.uint8)
    hsv[:, :, 0] = 255
    #hsv[:, :, 1] = 255
    mag, ang = cv2.cartToPolar(flow[..., 0], flow[..., 1])
    hsv[..., 0] = ang * 180 / np.pi / 2
    #hsv[..., 2] = cv2.normalize(mag, None, 0, 255, cv2.NORM_MINMAX)
    #rgb = cv2.cvtColor(hsv, cv2.COLOR_HSV2BGR)
    cv2.imwrite(args.output+'/'+args.save+'_flow.png', hsv)
cv2.imwrite(args.output+'/'+args.save+'_warped.png', im2W[:, :, ::-1] * 255)
