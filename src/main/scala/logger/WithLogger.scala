package logger

import org.slf4j.LoggerFactory

trait WithLogger {
  lazy val logger = LoggerFactory.getLogger(getClass)
}
