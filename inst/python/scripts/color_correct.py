import numpy, cv2

LEFT_EYE_POINTS = list(range(42, 48))
RIGHT_EYE_POINTS = list(range(36, 42))

def correct_colours(im1, im2, landmarks1, blur):
    img1 = cv2.imread(im1)
    img2 = cv2.imread(im2)
    COLOUR_CORRECT_BLUR_FRAC = blur

    blur_amount = COLOUR_CORRECT_BLUR_FRAC * numpy.linalg.norm(
                              numpy.mean(landmarks1[LEFT_EYE_POINTS], axis=0) -
                              numpy.mean(landmarks1[RIGHT_EYE_POINTS], axis=0))
    blur_amount = int(blur_amount)
    if blur_amount % 2 == 0:
        blur_amount += 1
    im1_blur = cv2.GaussianBlur(img1, (blur_amount, blur_amount), 0)
    im2_blur = cv2.GaussianBlur(img2, (blur_amount, blur_amount), 0)

    # Make sure we have correct dtype
    # im2_blur = numpy.int8(im2_blur)

    # Avoid divide-by-zero errors.
    im2_blur += (128 * (im2_blur <= 1.0)).astype(im2_blur.dtype)

    return (img2.astype(numpy.float64) * im1_blur.astype(numpy.float64) / im2_blur.astype(numpy.float64))
