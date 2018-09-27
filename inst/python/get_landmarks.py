import numpy
import dlib

class TooManyFaces(Exception):
    pass

class NoFaces(Exception):
    pass

def get_landmarks(im, PREDICTOR_PATH):
    detector = dlib.get_frontal_face_detector()
    predictor = dlib.shape_predictor(PREDICTOR_PATH)
    rects = detector(im, 1)
    if len(rects) > 1:
        return numpy.matrix([[p.x, p.y] for p in predictor(im, rects[0]).parts()])
    if len(rects) == 0:
        raise NoFaces
    return numpy.matrix([[p.x, p.y] for p in predictor(im, rects[0]).parts()])
