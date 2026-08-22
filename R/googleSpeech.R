#' Google Speech API Transcribe
#'
#' Wrapper around various Google packages to simplify speech transcription.
#'
#' @param filename Path to file for analysis
#' @param bucket Storage bucket on Google Cloud for larger files
#' @param ... Additional arguments to pass to gl_speech()
#' @importFrom tuneR readWave
#' @importFrom seewave duration savewav resamp
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
      message("Downsampling to 48kHz")
      #savewav()'s f argument writes a sample rate into the header, it does not
      #resample. Writing the original samples under a lower rate slowed the audio
      #down and dropped its pitch, and changed its duration. The file is also
      #written to a temporary path rather than into the working directory.
      wave <- seewave::resamp(wave, f=wave@samp.rate, g=max_samp_rate, output="Wave")
      filename <- tempfile(fileext=".wav")
      savewav(wave, f=max_samp_rate, filename=filename, extensible = FALSE)
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

gs_transcribe_execute <- function(object, max.tries=100, ...) {
  object
  async <- googleLanguageR::gl_speech(object, asynch=TRUE,...)
  async <- googleLanguageR::gl_speech_op(async)
  tries <- 1
  #gl_speech_op() hands back the operation for as long as it is still running, and
  #the transcript once it has finished, so the operation losing that class is what
  #says the work is done. The loop previously had no test at all and never ended,
  #which made the transcript below unreachable.
  while (inherits(async, "gl_speech_op")) {
    if (tries >= max.tries) {
      stop("Transcription had not finished after ", max.tries, " attempts.")
    }
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
