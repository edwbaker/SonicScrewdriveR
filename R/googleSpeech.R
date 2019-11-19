#' Google Speech API Transcribe
#'
#' Wrapper around various Google packages to simplify speech transcription.
#'
#' @param filename Path to file for analysis
#' @param bucket Storage bucket on Google Cloud for larger files
#' @param ... Additional arguments to pass to gl_speech()
#' @importFrom tuneR readWave
#' @importFrom seewave duration
#' @export
#' @return A gs_transcribe object containing details of the transcription
#' @examples
#' \dontrun{
#' gs_transcribe("demo.wav")
#' }
#'
gs_transcribe <- function(filename, bucket=NULL,...) {
  if (package.installed("googleCloudStorageR") & package.installed("googleLanguageR")) {
    max_d <- 3 #Max duration for objects not in Cloud Storage
    wave <- readWave(filename)
    if (duration(wave) < max_d) {
      return(gs_transcribe_execute(filename,...))
    } else {
      #Upload
      upload_try <- googleCloudStorageR::gcs_upload(filename, bucket=bucket, name="temp")
      result <- gs_transcribe_execute(paste0("gs://",bucket,"/temp"),...)
      #Cleanup
      googleCloudStorageR::gcs_delete_object("temp", bucket=bucket)
      return(result)
    }
  }
}

gs_transcribe_execute <- function(object,...) {
  async <- googleLanguageR::gl_speech(object, asynch=TRUE,...)
  result <- googleLanguageR::gl_speech_op(async)
  tries <- 1
  while (is.null(result$transcript)) {
    Sys.sleep(exponential_backoff(tries))
    result <- googleLanguageR::gl_speech_op(async)
    tries <- tries + 1
  }
  return(gs_preprocess_transcript(result))
}

gs_preprocess_transcript <- function(transcript, offset=0) {
  transcript$timings$startTime <- as.numeric(gsub("s", "", transcript$timings$startTime)) + offset
  transcript$timings$endTime <- as.numeric(gsub("s", "", transcript$timings$endTime)) + offset
  return(transcript)
}
