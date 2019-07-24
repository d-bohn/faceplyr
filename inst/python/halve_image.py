import cv2
import numpy as np
import os

def halve_face(image):
  # modified from original source
  # https://stackoverflow.com/questions/46712766/cropping-face-using-dlib-facial-landmarks
  
  path_sans_ext = print(os.path.splitext(image)[0])
  path_ext = print(os.path.splitext(image)[1])
  
  image = cv2.imread(image)

  height, width = image.shape[:2]
  
  # Let's get the starting pixel coordiantes (top left of cropped top)
  start_row, start_col = int(0), int(0)
  # Let's get the ending pixel coordinates (bottom right of cropped top)
  end_row, end_col = int(height * .5), int(width)
  cropped_top = image[start_row:end_row , start_col:end_col]
  # print start_row, end_row 
  # print start_col, end_col
  
  cv2.imwrite(path_sans_ext+"_top"+path_ext, cropped_top)

  # Let's get the starting pixel coordiantes (top left of cropped bottom)
  start_row, start_col = int(height * .5), int(0)
  # Let's get the ending pixel coordinates (bottom right of cropped bottom)
  end_row, end_col = int(height), int(width)
  cropped_bot = image[start_row:end_row , start_col:end_col]
  # print start_row, end_row 
  # print start_col, end_col
  cv2.imwrite(path_sans_ext+"_bottom"+path_ext, cropped_bot)
  
  return print("Images saved")
  
