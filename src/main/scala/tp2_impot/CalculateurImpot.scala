package tp2_impot

import tp2_impot.CalculateurImpot.{Tranche1, Tranche2, Tranche3, Tranche4}

trait CalculateurImpot:
  // Méthodes abstraites que les classes doivent fournir
  def revenuTotal: Double

  def nombreParts: Double

  // Méthodes concrètes qui utilisent les méthodes abstraites
  def calculerQuotientFamilial(): Double =
    if nombreParts > 0 then revenuTotal / nombreParts else 0.0

  def calculerTauxEffectif(impot: Double): Double =
    if revenuTotal > 0 then (impot / revenuTotal) * 100 else 0.0

  def calculerImpotProgressif(): Double = {
    val impotParPart = calculerQuotientFamilial() match
      case q if q < Tranche1 => 0
      case q if q < Tranche2 => (q - Tranche1) * .11
      case q if q < Tranche3 =>
        (q - Tranche2) * .30 + (Tranche2 - Tranche1) * .11
      case q if q < Tranche4 =>
        (q - Tranche3) * .41 + (Tranche3 - Tranche2) * .30 + (Tranche2 - Tranche1) * .11
      case q =>
        (q - Tranche4) * .45 + (Tranche4 - Tranche3) * .41 + (Tranche3 - Tranche2) * .30 + (Tranche2 - Tranche1) * .11
    impotParPart * nombreParts
  }

object CalculateurImpot:
  val Tranche1 = 11498
  val Tranche2 = 29316
  val Tranche3 = 83824
  val Tranche4 = 180295

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


