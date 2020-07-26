#' Calculate Facial Width-to-Height Ratio
#'
#' @param points Points returned by `read_landmarks` function
#' @param method either `average`, `left`, or `right` to determine which points on which side of the face to use for determining metrics.
#' @param top Determine top of face by either `eyebrow` or `eyelid`
#'
#' @return
#' @export
#'
#' @examples
calculate_fwhr <- function(points, method = "average", top = "eyebrow") {
  width_left <- points[points$point==0,c("x","y")]
  width_right <- points[points$point==16,c("x","y")]

  if (top == "eyebrow") {
    top_left <- points[points$point==18,c("x","y")]
    top_right <- points[points$point==25,c("x","y")]

  } else if (top == "eyelid") {
    top_left <- points[points$point==37,c("x","y")]
    top_right <- points[points$point==43,c("x","y")]

  } else {
    stop ("Invalid top point. Must be of value 'eyebrow' or 'eyelid'")
  }

  bottom_left <- points[points$point==50,c("x","y")]
  bottom_right <- points[points$point==52,c("x","y")]

  if (method == "left") {
    coords <- c(width_left[[1]], width_right[[1]], top_left[[2]], bottom_left[[2]])

  } else if (method == "right") {
    coords <- c(width_left[[1]], width_right[[1]], top_right[[2]], bottom_right[[2]])

  } else if (method == "average") {
    top_average <- (top_left [[2]] + top_right[[2]])/2
    bottom_average <- (bottom_left[[2]] + bottom_right[[2]])/2
    coords <- c(width_left[[1]], width_right[[1]], top_average, bottom_average)

  } else {
    stop ("'method' must be of value 'left', 'right', or 'average'")
  }

  if (top == "eyelid") {
    coords <- c(coords[[1]], coords[[2]], coords[[3]] - 4, coords[[4]])
  }

  top_left <- c(coords[[1]], coords[[3]])
  bottom_left <- c(coords[[1]], coords[[4]])
  top_right <- c(coords[[2]], coords[[3]])
  bottom_right <- c(coords[[2]], coords[[4]])

  width <- top_right[[1]] - top_left[[1]]
  height <- bottom_left[[2]] - top_left[[2]]

  fwhr <- width/height

  return(fwhr)
}
