library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

path <- "data-raw/qop/990/"

# Image file info
images <- read_csv(path %p% "IRS990-2017_12_PF/2017_12_PF/irs.2017_12_PF.dat.txt",
                 col_names = paste0("X", 1:1528))

# PDF file info
test2 <- read_csv(path %p% "IRS990-2017_12_PF/2017_12_PF/htaccess.2017_12_PF.txt",
                 col_names = FALSE)

images %<>%
  rename(
    a = X1,
    paper_or_electronic = X2,
    EIN = X3,
    date_of_form = X4,
    name = X5,
    state = X6,
    zip_code = X7,
    form = X8,
    num_ = X9,
    num2 = X10,
    date_uploaded = X11) %>%
  pivot_longer(X12:X1528, names_to = "file", values_to = "image_name") %>%
  filter(!is.na(name)) %>%
  mutate(image_name = str_sub(image_name, 4, 15)) %>%
  filter(form == "990PF") %>%
  group_by(EIN) %>%
  summarise(image_name = first(image_name))

all_pdfs <- list.files(path %p% "IRS990-2017_12_PF/2017_12_PF")
all_pdfs %<>% paste0(path, "IRS990-2017_12_PF/2017_12_PF/", .)


library(tesseract)
library(pdftools)
library(imager)
library(magick)

test <- image_read(path %p% "IRS990-2017_12_PF/2017_12_PF_01/" %p% images$image_name[1])
test2 <- magick2cimg(test)

text <- ocr(path %p% "IRS990-2017_12_PF/2017_12_PF_01/" %p% images$image_name[1], HOCR = T) %>%
  hocr::hocr_parse() %>%
  hocr::tidy_tesseract()

library(ggplot2)

p1 <- text %>%
  mutate(ocrx_word_bbox=lapply(ocrx_word_bbox, function(x)
    separate(as_tibble(x), value, into=c("word_x1", "word_y1", "word_x2", "word_y2"), convert = TRUE))) %>%
  unnest(ocrx_word_bbox) %>%
  mutate(ocr_page_bbox=lapply(ocr_page_bbox, function(x)
    separate(as_tibble(x), value, into=c("page_x1", "page_y1", "page_x2", "page_y2"), convert = TRUE))) %>%
  unnest(ocr_page_bbox) %>%
  mutate(word_y1=max(page_y2)-word_y1,
         word_y2=max(page_y2)-word_y2) %>%
  ggplot(aes(xmin=word_x1, ymin=word_y1, xmax=word_x2, ymax=word_y2))+
  geom_rect(aes(color=ocr_par_id, fill=ocrx_word_conf), show.legend = TRUE)+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7))

test3 <- as.cimg.raster(test2)

p2 <- grid::rasterGrob(test3, interpolate=TRUE)

p1

image_ggplot







library(OpenImageR)

im1 = readImage(path %p% "IRS990-2017_12_PF/2017_12_PF_01/" %p% images$image_name[1], native = TRUE)
im2 = readImage(path %p% "IRS990-2017_12_PF/2017_12_PF_01/" %p% images$image_name[1], convert = TRUE)

edsc = edge_detection(im2, method = 'Scharr', conv_mode = 'same')

imageShow(edsc)


res_slic = superpixels(input_image = im1,
                       method = "slico",
                       superpixel = 200,
                       compactness = 20,
                       return_slic_data = TRUE,
                       return_labels = TRUE,
                       write_slic = "",
                       verbose = TRUE)












for(bbox in 1:483) {

  print(bbox)

  this_line <- p1[bbox,]

  test2 <- draw_rect(test2, this_line$word_x1, this_line$word_y1, this_line$word_x2, this_line$word_y2,
                     color = "black", opacity = 1, filled = F)
}


draw_rect <- function (im, x0, y0, x1, y1, color = "white", opacity = 1, filled = TRUE) {
  if (is.character(color)) {
    color <- col2rgb(color)[, 1]/255
  }
  ls <- purrr::map_int(c(x0, y0, x1, y1), length)
  if (min(ls) != max(ls))
    stop("x0,y0,x1,y1 must be the same length")
  imager:::draw_rect_(im, x0, y0, x1, y1, color, opacity, filled)
}

plot(test2)


test <- str_split(text, "\n")

test <- test[[1]]

test <- test[str_detect(test, "8 Contributions and grants")]

test <- str_split(test, " ")

test <- test[[1]]

test <- test[(length(test)-1):length(test)]

test <- str_replace_all(test, ",", "")

test <- as.numeric(test)













pngfile <- pdftools::pdf_convert(all_pdfs[1], dpi = 300)

pngfile <- pngfile[2]

text <- ocr_data(pngfile)

test <- str_split(text, "\n")

test <- test[[1]]

test <- test[str_detect(test, "8 Contributions and grants")]

test <- str_split(test, " ")

test <- test[[1]]

test <- test[(length(test)-1):length(test)]

test <- str_replace_all(test, ",", "")

test <- as.numeric(test)



test <- str_split(text, "\n")

test <- test[[1]]

test <- test[str_detect(test, "8 Contributions and grants")]

test <- str_split(test, " ")

test <- test[[1]]

test <- test[(length(test)-1):length(test)]

test <- str_replace(test, ",", "")

test <- as.numeric(test)



