import other.Curve.EccLength

package object other {

  type HexString = String

  case class MessageContext()
  case class Eid(value: String)

  sealed trait ES7Command extends Command

  // Provider (smsr1)
  case class AuthenticateSMSR(messageContext: MessageContext, eid: Eid, smsrCertificate: HexString) extends ES7Command
  case class CreateAdditionalKeySet(messageContext: MessageContext, eid: Eid, keyVersionNumber: Int, initialSequenceCounter: Int, curve: Curve, scenarioParameter: Int, hostId: Option[HexString], ephemeralPublicKey: HexString, signature: HexString) extends ES7Command
  case class HandoverEuicc(messageContext: MessageContext, eid: Eid, eis: Eis) extends ES7Command

  // Requester (smsr2)
  case class AuthenticateSMSRCallback(messageContext: MessageContext, eid: Eid, randomChallenge: Option[HexString]) extends ES7Command
  case class CreateAdditionalKeySetCallback(messageContext: MessageContext, eid: Eid, derivationRandom: HexString, receipt: HexString) extends ES7Command
  case class HandoverEuiccCallback(messageContext: MessageContext, eid: Eid) extends ES7Command

  sealed trait ES4Command extends Command

  case class PrepareSMSRChange(messageContext: MessageContext, eid: Eid, currentSMSRid: String) extends ES4Command
  case class SMSRChange(messageContext: MessageContext, eid: Eid, targetSmsrId: String) extends ES4Command

  trait Command {
    val messageContext: MessageContext
  }

  case class Eis(/* This is eUICC representation...  biiiig object, so i'll just skip it */)

  // Curve
  sealed case class Curve(tag: HexString, name: String, oid: String, length: EccLength)
  object Curve {
    val values = Seq(NIST_P_256, BRAINPOOLP256R1, FRP256V1)

    def getByOid(oid: String): Option[Curve] = values.find(_.oid == oid)

    /**
      * The length of the Elliptic Curve Cryptography key
      */
    sealed trait EccLength {
      val value: String
      val count: Int
    }
    case object ECC_256 extends EccLength {
      val value = "ECC-256"
      val count = 256
    }
    case object ECC_354 extends EccLength {
      val value = "ECC-354"
      val count = 354
    }
    case object ECC_512 extends EccLength {
      val value = "ECC-512"
      val count = 512
    }
    case object ECC_521 extends EccLength {
      val value = "ECC-521"
      val count = 521
    }

    object EccLength {

      private val keyValueMapping: Map[String, EccLength] = Map(
        ECC_256.value -> ECC_256,
        ECC_354.value -> ECC_354,
        ECC_512.value -> ECC_512,
        ECC_521.value -> ECC_521
      )

      def valueOf(str: String): EccLength = {
        keyValueMapping.getOrElse(str, throw new NoSuchElementException(s"There is no EccLength definition for value $str"))
      }
    }

    object NIST_P_256 extends Curve("00", "NIST P-256", "1.2.840.10045.3.1.7", ECC_256)
    object BRAINPOOLP256R1 extends Curve("03", "brainpoolP256r1", "1.3.36.3.3.2.8.1.1.7", ECC_256)
    object FRP256V1 extends Curve("40", "FRP256V1", "1.2.250.1.223.101.256.1", ECC_256)
  }
}
