import cv2
import mahotas as mt
  
def face_hist(img, shape):
  image = cv2.imread(img)
  
  hist = cv2.calcHist([image], [0, 1, 2], None, shape, [0, 256, 0, 256, 0, 256])
  hist = hist.flatten()
  return hist

def face_texture(img):
  image = cv2.imread(img)
  
  gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
  # calculate haralick texture features for 4 types of adjacency
  textures = mt.features.haralick(gray)
  
  # take the mean of it and return it
  ht_mean = textures.mean(axis=0)
  return ht_mean
