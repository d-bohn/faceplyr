transform_image = 'test.jpg'
left_same = TRUE
right_window = 'test_angry.jpg'
create_superbatch_jpm <- function(transform_image,
                                  left_same = TRUE,
                                  left_window,
                                  right_window,
                                  ...) {
  dots <- list(...)

  batch <- data.frame(
    "subject_file_name" =                             # (i.e. Transform window)
      as.character(transform_image),
    "source_file_name" =                              # (i.e. left hand window)
      as.character(if (left_same) {
        transform_image
      } else {
        left_window
      }),
    "target_file_name" =                              # (i.e. right hand window)
      as.character(right_window),
    "start_amount" =
      as.numeric(if (hasArg(start_amount)) {
        dots$start_amount
      } else {
        0
      }),
    "end_amount" =
      as.numeric(if (hasArg(end_amount)) {
        dots$end_amount
      } else {
        1
      }),
    "number_of_steps" =
      as.numeric(if (hasArg(number_of_steps)) {
        dots$number_of_steps
      } else {
        101
      }),
    "output_file_name" =
      as.character(if (!hasArg(output_file_name)) {
        paste0('transformed_', transform_image)
      } else {
        dots$output_file_name
      }),
    "shape_boolean_value" = as.numeric(if (hasArg(shape_boolean_value)) {
      dots$shape_boolean_value
    } else {
      1
    }),
    "colour_boolean_value" = as.numeric(if (hasArg(colour_boolean_value)) {
      dots$colour_boolean_value
    } else {
      1
    }),
    "texture_boolean_value" = as.numeric(if (hasArg(texture_boolean_value)) {
      dots$texture_boolean_value
    } else {
      1
    }),
    "mask_boolean_value" = as.numeric(if (hasArg(mask_boolean_value)) {
      dots$mask_boolean_value
    } else {
      0
    }),
    "sample_boolean_value" = as.numeric(if (hasArg(sample_boolean_value)) {
      dots$sample_boolean_value
    } else {
      1
    }),
    "resize_scale" = as.numeric(if (hasArg(resize_scale)) {
      dots$resize_scale
    } else {
      1
    })
    )
    return(batch)
  }

create_superbatch_jpm(transform_image, left_same=1, right_window=right_window)
