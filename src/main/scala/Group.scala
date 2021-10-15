class Group(
    val name: String,
    val people: List[Person],
    vMap: Map[String, Vaccine],
    eSet: Set[String]
):
  val vList: List[(Person, Option[Vaccine])] =
    people.map(p => p -> vMap.get(p.number))

  val (erList, nrList): (List[Person], List[Person]) =
    vList.collect { case (p, None) => p }.partition(p => eSet(p.number))

  val nraList: List[Person] = nrList.filter(_.active)

  val rList: List[(Person, Vaccine)] =
    vList.collect { case (p, Some(v)) => p -> v }

  def total: Double = people.length.toDouble
  def notResponded: Double = nrList.length.toDouble
  def notRespondedActive: Double = nraList.length.toDouble
  def errResponded: Double = erList.length.toDouble
  def responded: Double = rList.length.toDouble
  def unvaccinated: Double = rList.count(_._2.unvaccinated)
  def ongoing: Double = rList.count(_._2.ongoing)
  def completed: Double = rList.count(_._2.completed)

  def getStat: List[Double] =
    List(
      total,
      notResponded,
      notRespondedActive,
      errResponded,
      responded,
      unvaccinated,
      ongoing,
      completed
    )
