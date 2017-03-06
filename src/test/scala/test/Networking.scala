package test

import java.net.{InetSocketAddress, Socket}

import logger.WithLogger


object Networking extends WithLogger {

  def testInet(site: String): Boolean = {
    val sock = new Socket();
    val addr = new InetSocketAddress(site, 80);
    try {
      sock.connect(addr, 3000);
      true
    } catch {
      case e: Exception => {
        logger.error(s"Error when trying to verify internet connection",e)
        false
      }
    } finally {
      try {
        sock.close()
      }
    }
  }

}
