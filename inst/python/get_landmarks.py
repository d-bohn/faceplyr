import numpy
import dlib
import cv2

# class TooManyFaces(Exception):
    # None

# class NoFaces(Exception):
    # None

def get_landmarks(im, PREDICTOR_PATH):
    if isinstance(im, str):
        img = cv2.imread(im)
    
    detector = dlib.get_frontal_face_detector()
    predictor = dlib.shape_predictor(PREDICTOR_PATH)
    rects = detector(img, 0)
    # if len(rects) > 1:
    #    return numpy.matrix([[p.x, p.y] for p in predictor(im, rects[0]).parts()])
    if len(rects) == 0:
        # raise NoFaces
        return None
    return numpy.matrix([[p.x, p.y] for p in predictor(img, rects[0]).parts()])
