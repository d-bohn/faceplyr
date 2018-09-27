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
import cv2

parser = argparse.ArgumentParser(
    description='python wrapper for viz Coarse2Fine Optical Flow')
parser.add_argument(
    '-r', dest='reference', action='store', required=True)
parser.add_argument(
    '-s', dest='save', action='store', required=True)
parser.add_argument(
    '-o', dest='output', action='store', required=True)
parser.add_argument(
    '-f', dest='flow', action='store', required=True)
args = parser.parse_args()

flow = np.load(args.flow)

im1 = np.array(Image.open(args.reference))
if len(im1.shape) != 3 :
    im1 = np.expand_dims(im1, axis=2)

hsv = np.zeros(im1.shape, dtype=np.uint8)
hsv[:, :, 0] = 255
#hsv[:, :, 1] = 255
mag, ang = cv2.cartToPolar(flow[..., 0], flow[..., 1])
hsv[..., 0] = ang * 180 / np.pi / 2
#hsv[..., 2] = cv2.normalize(mag, None, 0, 255, cv2.NORM_MINMAX)
#rgb = cv2.cvtColor(hsv, cv2.COLOR_HSV2BGR)
cv2.imwrite(args.output+'/'+args.save+'_flow.png', hsv)
#cv2.imwrite(args.output+'/'+args.save+'_warped.png', im2W[:, :, ::-1] * 255)
