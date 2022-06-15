package de.tubs.ias.ledeco.tracker.identification

import de.tubs.ias.ledeco.database.entities.{CellphoneApplication, Request}

case class AppTrackingIdIsolationSet(app: CellphoneApplication,
                                     firstPhoneFirstRun: Seq[Request],
                                     firstPhoneSecondRun: Seq[Request],
                                     secondPhoneFirstRun: Seq[Request],
                                     secondPhoneSecondRun: Seq[Request]) {

  def getLengthString: String =
    s"{${firstPhoneFirstRun.length}/${firstPhoneSecondRun.length}}/{${secondPhoneFirstRun.length}/${secondPhoneSecondRun.length}}"

}
