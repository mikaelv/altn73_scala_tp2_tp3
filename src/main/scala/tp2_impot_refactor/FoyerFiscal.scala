package tp2_impot_refactor

sealed trait FoyerFiscal:
  def revenuTotal: Double
  def nombreParts: Double

// Partie 6
sealed trait FoyerSansEnfants extends FoyerFiscal

case class Celibataire(nom: String, revenuTotal: Double)
    extends FoyerSansEnfants:
  def nombreParts: Double = 1

case class Couple(p1: Celibataire, p2: Celibataire) extends FoyerSansEnfants:
  // Constructeur additionnel
  def this(nom1: String, nom2: String, revenu1: Double, revenu2: Double) =
    this(Celibataire(nom1, revenu1), Celibataire(nom2, revenu2))

  def nombreParts: Double = p1.nombreParts + p2.nombreParts

  def revenuTotal: Double = p1.revenuTotal + p2.revenuTotal

case class FoyerAvecEnfants(foyerParent: FoyerSansEnfants, nombreEnfants: Int)
    extends FoyerFiscal:
  def revenuTotal: Double = foyerParent.revenuTotal

  override def nombreParts: Double =
    val partsParent = foyerParent match
      case _: Celibataire => foyerParent.nombreParts + 0.5
      case _              => foyerParent.nombreParts

    val partsEnfants =
      if nombreEnfants <= 2 then .5 * nombreEnfants
      else 1.0 + nombreEnfants - 2

    partsParent + partsEnfants
