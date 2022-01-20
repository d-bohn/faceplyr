import numpy as np
import dlib
import cv2
from imutils import face_utils
import os

def p_poly_area (x, y):
  return 0.5*np.abs(np.dot(x,np.roll(y,1))-np.dot(y,np.roll(x,1)))

def p_read_landmarks(im, PREDICTOR_PATH):
    if isinstance(im, str):
        img = cv2.imread(im)
    
    detector = dlib.get_frontal_face_detector()
    predictor = dlib.shape_predictor(PREDICTOR_PATH)
    rects = detector(img, 0)
    
    if len(rects) > 1:
        return None
    if len(rects) == 0:
        # raise NoFaces
        return None
    
    return np.matrix([[p.x, p.y] for p in predictor(img, rects[0]).parts()])

def face_remap(shape):
    remapped_image = shape.copy()
    # left eye brow
    remapped_image[17] = shape[26]
    remapped_image[18] = shape[25]
    remapped_image[19] = shape[24]
    remapped_image[20] = shape[23]
    remapped_image[21] = shape[22]
    # right eye brow
    remapped_image[22] = shape[21]
    remapped_image[23] = shape[20]
    remapped_image[24] = shape[19]
    remapped_image[25] = shape[18]
    remapped_image[26] = shape[17]

    # neatening
    remapped_image = cv2.convexHull(shape)
    return remapped_image

def p_mask_face(image, landmarks):
  # modified from original source
  # https://stackoverflow.com/questions/46712766/cropping-face-using-dlib-facial-landmarks

    if isinstance(image, str):
        image = cv2.imread(image)
    else:
        image = image

    # image = imutils.resize(image, width=500)
    gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)

    out_face = np.zeros_like(image)

    # initialize dlib's face detector (HOG-based) and then create the facial landmark predictor
    detector = dlib.get_frontal_face_detector()
    predictor = dlib.shape_predictor(landmarks)

    # detect faces in the grayscale image
    rects = detector(gray, 1)

    # loop over the face detections
    for (i, rect) in enumerate(rects):
        shape = predictor(gray, rect)
        shape = face_utils.shape_to_np(shape)

        #initialize mask array
        remapped_shape = np.zeros_like(shape)
        feature_mask = np.zeros((image.shape[0], image.shape[1]))

        # we extract the face
        remapped_shape = face_remap(shape)
        cv2.fillConvexPoly(feature_mask, remapped_shape[0:27], 1)
        feature_mask = feature_mask.astype(np.bool)
        out_face[feature_mask] = image[feature_mask]

    return out_face

def p_crop_background(image):
  # modified from original source
  # https://stackoverflow.com/questions/46712766/cropping-face-using-dlib-facial-landmarks

    if isinstance(image, str):
        image = cv2.imread(image)
    else:
        image = image

    gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
    th, threshed = cv2.threshold(gray, 1, 255, cv2.THRESH_BINARY)

    ## (2) Morph-op to remove noise
    kernel = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (11,11))
    morphed = cv2.morphologyEx(threshed, cv2.MORPH_CLOSE, kernel)

    ## (3) Find the max-area contour
    cnts = cv2.findContours(morphed, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)[-2]
    cnt = sorted(cnts, key=cv2.contourArea)[-1]

    ## (4) Crop and save it
    x,y,w,h = cv2.boundingRect(cnt)
    dst = image[y:y+h, x:x+w]

    return dst
    
def p_remove_background(image):
    
    if isinstance(image, str):
        image = cv2.imread(image)
    else:
        image = image
    
    tmp = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
    _,alpha = cv2.threshold(tmp,0,255,cv2.THRESH_BINARY)
    b, g, r = cv2.split(image)
    rgba = [b,g,r, alpha]
    dst = cv2.merge(rgba,4)
    
    return dst


def p_halve_face(image):
  # modified from original source
  # https://stackoverflow.com/questions/46712766/cropping-face-using-dlib-facial-landmarks

  path_sans_ext = os.path.splitext(image)[0]
  path_ext = os.path.splitext(image)[1]

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

  print("Images saved")
