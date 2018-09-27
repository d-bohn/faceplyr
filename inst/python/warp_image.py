import cv2, numpy

def warp_im(im, M, width, height):
    img = cv2.imread(im)
    # height, width, channels = img.shape
    # dshape = [height, width]
    output = numpy.zeros([width, height], dtype=img.dtype)

    output = cv2.warpAffine(
          src = img,
          dst = output,
          M = M[:2],
          dsize = (width, height),
          borderMode = cv2.BORDER_TRANSPARENT,
          flags = cv2.WARP_INVERSE_MAP)

    return output
