;Code to organise the .jpg files in specified source folder based on date taken of the photo
;in the specified destination folder uses metadata-extractor to extract the embedded date
;information in the photo refer http://code.google.com/p/metadata-extractor
(ns org-files
(:use clojure.java.io)
  (import (com.drew.imaging ImageMetadataReader)
    (com.drew.metadata Directory Metadata Tag)
    (com.drew.metadata.exif ExifIFD0Directory)))
 
(def formatter (new java.text.SimpleDateFormat "dd_MMM_yyyy"))
(def fs java.io.File/separator)

(defn name [file] (. file getName))

(defn get-image-date [file]
  "Get the timestamp of the photo taken date from the JPG file metadata"
  (let [metadata (ImageMetadataReader/readMetadata file)
        dir (. metadata getDirectory ExifIFD0Directory)
        dt (. dir getDate ExifIFD0Directory/TAG_DATETIME)]
    dt))

(defn folder? [file-name]
  "Check whether the given folder is valid"
  (let [file-obj (file file-name)]
  (and (. file-obj exists) (. file-obj isDirectory))))

(defn filter-file [f]
  "Filters files only with extension .jpg from a list of files"
  (and
    (. (. f getName) endsWith ".jpg")
    (not (folder? f))))

(defn files [folder]
  "List all the files in given folder as a seq"
  (if (not (folder? folder))
    (throw (new Exception (str "Error! " folder " is not valid!")))
  (filter filter-file (seq (. (file folder) listFiles)))))
 
(defn get-folder [file-name]
  "Get the folder name based on specified format and date of photo taken"
  (let [f (file file-name)
        date (get-image-date f)]
  (. formatter format date)))
 
(defn copy-file [target-folder cur-file]
  "Copy the contents from source to target"
  (let [fname (str target-folder fs (get-folder cur-file) fs (name cur-file))]
  (make-parents fname)
  (copy cur-file (file fname))
  fname))
 
(defn organize-files [src target]
  "Organize the files into folders based on the photo taken date"
  (println (map (partial copy-file target) (files src))))

(organize-files "/media/CABC486FBC485859/Pics" "/home/raam/Adithya")
