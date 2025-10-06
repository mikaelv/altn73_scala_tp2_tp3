package tp2_impot_refactor

object CalculateurImpot:
  val Tranche1 = 11498
  val Tranche2 = 29316
  val Tranche3 = 83824
  val Tranche4 = 180295

  // Méthodes concrètes qui utilisent les méthodes abstraites
  def calculerQuotientFamilial(foyer: FoyerFiscal): Double =
    if foyer.nombreParts > 0 then foyer.revenuTotal / foyer.nombreParts else 0.0

  def calculerTauxEffectif(foyer: FoyerFiscal, impot: Double): Double =
    if foyer.revenuTotal > 0 then (impot / foyer.revenuTotal) * 100 else 0.0

  def calculerImpotProgressif(foyer: FoyerFiscal): Double = {
    val impotParPart = calculerQuotientFamilial(foyer) match
      case q if q < Tranche1 => 0
      case q if q < Tranche2 => (q - Tranche1) * .11
      case q if q < Tranche3 =>
        (q - Tranche2) * .30 + (Tranche2 - Tranche1) * .11
      case q if q < Tranche4 =>
        (q - Tranche3) * .41 + (Tranche3 - Tranche2) * .30 + (Tranche2 - Tranche1) * .11
      case q =>
        (q - Tranche4) * .45 + (Tranche4 - Tranche3) * .41 + (Tranche3 - Tranche2) * .30 + (Tranche2 - Tranche1) * .11
    impotParPart * foyer.nombreParts
  }


  // V2 : récursion
  def calculerImpotParPart(revenuParPart: Double): Double =
    revenuParPart match
      case q if q < Tranche1 => 0
      case q if q < Tranche2 => (q - Tranche1) * .11
      case q if q < Tranche3 =>
        (q - Tranche2) * .30 + calculerImpotParPart(Tranche2)
      case q if q < Tranche4 =>
        (q - Tranche3) * .41 + calculerImpotParPart(Tranche3)
      case q => (q - Tranche4) * .45 + calculerImpotParPart(Tranche4)


