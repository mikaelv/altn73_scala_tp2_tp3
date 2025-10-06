package tp2_impot

import CalculateurImpot.{Tranche1, Tranche2, Tranche3, Tranche4}

sealed trait FoyerFiscal:
  def revenuTotal: Double
  def nombreParts: Double

trait CalculateurImpot:
  // Méthodes abstraites que les classes doivent fournir
  def revenuTotal: Double

  def nombreParts: Double

  // Méthodes concrètes qui utilisent les méthodes abstraites
  def calculerQuotientFamilial(): Double =
    if nombreParts > 0 then revenuTotal / nombreParts else 0.0

  def calculerTauxEffectif(impot: Double): Double =
    if revenuTotal > 0 then (impot / revenuTotal) * 100 else 0.0

  def calculerImpotProgressif(): Double =
    calculerQuotientFamilial() match
      case q if q < Tranche1 => 0
      case q if q < Tranche2 => (q - Tranche1) * .11
      case q if q < Tranche3 =>
        (q - Tranche2) * .30 + (Tranche2 - Tranche1) * .11
      case q if q < Tranche4 =>
        (q - Tranche3) * .41 + (Tranche3 - Tranche2) * .30 + (Tranche2 - Tranche1) * .11
      case q =>
        (q - Tranche4) * .45 + (Tranche4 - Tranche3) * .41 + (Tranche3 - Tranche2) * .30 + (Tranche2 - Tranche1) * .11

object CalculateurImpot:
  val Tranche1 = 11498
  val Tranche2 = 29316
  val Tranche3 = 83824
  val Tranche4 = 180295

  // V2 : récursion
  def calculerImpotProgressif(revenuParPart: Double): Double =
    revenuParPart match
      case q if q < Tranche1 => 0
      case q if q < Tranche2 => (q - Tranche1) * .11
      case q if q < Tranche3 =>
        (q - Tranche2) * .30 + calculerImpotProgressif(Tranche2)
      case q if q < Tranche4 =>
        (q - Tranche3) * .41 + calculerImpotProgressif(Tranche3)
      case q => (q - Tranche4) * .45 + calculerImpotProgressif(Tranche4)

case class Celibataire(nom: String, revenuTotal: Double)
    extends FoyerFiscal
    with CalculateurImpot:
  override def nombreParts: Double = 1
  

@main
def main(): Unit =
  val pierre = Celibataire("Pierre Dupont", 35000.0)
  println(s"${pierre.nom}: ${pierre.revenuTotal}€, ${pierre.nombreParts} parts")
// Sortie attendue: Pierre Dupont: 35000.0€, 1.0 parts

  val marie = Celibataire("Marie Martin", 40000.0)
  println(f"${marie.nom}:")
  println(f"  Quotient familial: ${marie.calculerQuotientFamilial()}%.2f€")
  println(f"  Taux effectif: ${marie.calculerTauxEffectif(6000)}%.2f%%")
  // Sortie attendue:
  // Marie Martin:
  //   Quotient familial: 40000.00€
  //   Impôt à 15%: 6000.00€
  //   Taux effectif: 15.00%
  val testeurs = List(
    Celibataire("Paul", 8000.0), // Exonéré
    Celibataire("Sophie", 20000.0), // Tranche 11%
    Celibataire("Jean", 50000.0), // Tranche 30%
    Celibataire("Claire", 100000.0) // Tranche 41%
  )

  println("=== Calcul progressif ===")
  testeurs.foreach { foyer =>
    val impot = foyer.calculerImpotProgressif()
    val taux = foyer.calculerTauxEffectif(impot)
    println(f"${foyer.nom}: ${impot}%.2f€ (${taux}%.2f%%)")
  }
