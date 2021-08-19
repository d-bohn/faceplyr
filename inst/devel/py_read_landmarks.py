import numpy
import dlib
import cv2

class face_error(Exception):
  pass

def py_read_landmarks(im, PREDICTOR_PATH, height_resize = 480):
  
  detector = dlib.get_frontal_face_detector()
  predictor = dlib.shape_predictor(PREDICTOR_PATH)
  
  if isinstance(im, str):
    img = cv2.imread(im)
  
  # Preprocess the image to make faster
  height = img.shape[0]
  # calculate resize scale
  frame_resize_scale = float(height)/height_resize
  size = img.shape[0:2]  
  
  img_small = cv2.resize(img, None, fx = 1.0/frame_resize_scale, fy = 1.0/frame_resize_scale, interpolation = cv2.INTER_LINEAR)
  
  faces = detector(img_small, 0)
  if len(faces) > 1:
     raise too_many_faces("Only returning first face coordinates!") 
    
  if len(faces) == 0:
      # raise NoFaces
      raise too_many_faces("No faces found!") 
    

  new_rects = dlib.rectangle(int(faces[0].left() * frame_resize_scale),
                             int(faces[0].top() * frame_resize_scale),
                             int(faces[0].right() * frame_resize_scale),
                             int(faces[0].bottom() * frame_resize_scale))
  
    
  return numpy.matrix([[p.x, p.y] for p in predictor(img, new_rects).parts()])
