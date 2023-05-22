#' Google Speech API Transcribe
#'
#' Wrapper around various Google packages to simplify speech transcription.
#'
#' @param filename Path to file for analysis
#' @param bucket Storage bucket on Google Cloud for larger files
#' @param ... Additional arguments to pass to gl_speech()
#' @importFrom tuneR readWave
#' @importFrom seewave duration savewav
#' @export
#' @return A gs_transcribe object containing details of the transcription
#' @examples
#' \dontrun{
#' gs_transcribe("demo.wav")
#' }
#'
gs_transcribe <- function(filename, bucket=NULL,...) {
  if (package.installed("googleCloudStorageR") & package.installed("googleLanguageR")) {
    max_d <- 3000 #Max duration for objects not in Cloud Storage
    max_samp_rate = 48000
    wave <- readWave(filename)
    if (wave@samp.rate > max_samp_rate) {
      print("Downsampling to 48kHz")
      savewav(wave, f=max_samp_rate, filename="temp.wav", extensible = FALSE)
      filename <- "temp.wav"
      wave@samp.rate <- max_samp_rate
    }
    if (duration(wave) < max_d) {
      return(gs_transcribe_execute(filename, ...))
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

gs_transcribe_execute <- function(object, ...) {
  object
  async <- googleLanguageR::gl_speech(object, asynch=TRUE,...)
  async <- googleLanguageR::gl_speech_op(async)
  tries <- 1
  while (TRUE) {
    Sys.sleep(exponential_backoff(tries))
    async <- googleLanguageR::gl_speech_op(async)
    tries <- tries + 1
  }
  return(gs_preprocess_transcript(async))
}

gs_preprocess_transcript <- function(transcript, offset=0) {
  transcript$timings$startTime <- as.numeric(gsub("s", "", transcript$timings$startTime)) + offset
  transcript$timings$endTime <- as.numeric(gsub("s", "", transcript$timings$endTime)) + offset
  return(transcript)
}
