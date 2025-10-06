package tp2_impot

sealed trait FoyerFiscal:
  def revenuTotal: Double
  def nombreParts: Double

// Partie 6 
sealed trait FoyerSansEnfants extends FoyerFiscal
  
case class Celibataire(nom: String, revenuTotal: Double)
  extends FoyerSansEnfants
    with CalculateurImpot:
  override def nombreParts: Double = 1

case class Couple(p1: Celibataire, p2: Celibataire)
  extends FoyerSansEnfants
    with CalculateurImpot:
  // Constructeur additionnel
  def this(nom1: String, nom2: String, revenu1: Double, revenu2: Double) =
    this(Celibataire(nom1, revenu1), Celibataire(nom2, revenu2))

  override def nombreParts: Double = p1.nombreParts + p2.nombreParts

  override def revenuTotal: Double = p1.revenuTotal + p2.revenuTotal

case class FoyerAvecEnfants(foyerParent: FoyerSansEnfants, nombreEnfants: Int)
  extends FoyerFiscal
    with CalculateurImpot:
  override def revenuTotal: Double = foyerParent.revenuTotal

  override def nombreParts: Double =
    val partsParent = foyerParent match
      case _: Celibataire => foyerParent.nombreParts + 0.5
      case _ => foyerParent.nombreParts

    val partsEnfants =
      if nombreEnfants <= 2 then.5 * nombreEnfants
      else 1.0 + nombreEnfants - 2

    partsParent + partsEnfants

