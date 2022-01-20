import cv2
import mahotas as mt
import numpy as np
  
def p_face_hist(img, shape, colorspace):
  img_read = cv2.imread(img)
  
  if colorspace == "rgb":
    image = img_read
    # grab the image channels, initialize the tuple of colors,
    # the figure and the flattened feature vector
    chans = cv2.split(image)
    colors = ("b", "g", "r")
    ranges = [[0,256],[0,256],[0,256]]
  
  if colorspace == "hsv":
    image = cv2.cvtColor(img_read, cv2.COLOR_BGR2HSV)
    chans = cv2.split(image)
    colors = ("h","s","v")
    ranges = [[0,180],[0,256],[0,256]]
 
  features = []
 
  # loop over the image channels
  for (chan, color, bins, ran) in zip(chans, colors, shape, ranges):
	  # create a histogram for the current channel and
	  # concatenate the resulting histograms for each
	  # channel
	  
	  hist = cv2.calcHist([chan], [0], None, [bins], ran)
	  features.extend(hist)
 
	  # plot the histogram
	  # plt.plot(hist, color = color)
	  # plt.xlim([0, 256])
  
  hist_ret = np.array(features).flatten()
  # hist = cv2.calcHist([image], [0, 1, 2], None, shape, [0, 256, 0, 256, 0, 256])
  # hist = hist.flatten()
  return hist_ret

def p_face_texture(img):
  image = cv2.imread(img)
  
  gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
  # calculate haralick texture features for 4 types of adjacency
  textures = mt.features.haralick(gray)
  
  # take the mean of it and return it
  ht_mean = textures.mean(axis=0)
  return ht_mean
