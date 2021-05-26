package lidraughts.tournament
package crud

import BSONHandlers._
import org.joda.time.DateTime

import draughts.variant.{ Brazilian, Russian, Standard }
import lidraughts.common.paginator.Paginator
import lidraughts.db.dsl._
import lidraughts.db.paginator.Adapter
import lidraughts.user.User

final class CrudApi {

  def list = TournamentRepo uniques 50

  def one(id: String) = TournamentRepo uniqueById id

  def editForm(tour: Tournament) = CrudForm.apply fill CrudForm.Data(
    name = tour.name,
    homepageHours = ~tour.spotlight.flatMap(_.homepageHours),
    clockTime = tour.clock.limitInMinutes,
    clockIncrement = tour.clock.incrementSeconds,
    minutes = tour.minutes,
    variant = tour.variant.id,
    positionStandard = if (tour.variant.standard) tour.positionKey.some else Standard.initialFen.some,
    positionRussian = if (tour.variant.russian) tour.positionKey.some else Russian.initialFen.some,
    positionBrazilian = if (tour.variant.brazilian) tour.positionKey.some else Brazilian.initialFen.some,
    date = tour.startsAt,
    image = ~tour.spotlight.flatMap(_.iconImg),
    headline = tour.spotlight.??(_.headline),
    description = tour.spotlight.??(_.description),
    conditions = Condition.DataForm.AllSetup(tour.conditions),
    password = tour.password,
    berserkable = !tour.noBerserk,
    streakable = !tour.noStreak,
    teamBattle = tour.isTeamBattle
  )

  def update(old: Tournament, data: CrudForm.Data) =
    TournamentRepo update updateTour(old, data) void

  def createForm = CrudForm.apply

  def create(data: CrudForm.Data, owner: User): Fu[Tournament] = {
    val tour = updateTour(empty, data).copy(createdBy = owner.id)
    TournamentRepo insert tour inject tour
  }

  def clone(old: Tournament) = old.copy(
    name = s"${old.name} (clone)",
    startsAt = DateTime.now plusDays 7
  )

  def paginator(page: Int) = Paginator[Tournament](adapter = new Adapter[Tournament](
    collection = TournamentRepo.coll,
    selector = TournamentRepo.selectUnique,
    projection = $empty,
    sort = $doc("startsAt" -> -1)
  ), currentPage = page)

  private def empty = Tournament.make(
    by = Left(User.lidraughtsId),
    name = none,
    clock = draughts.Clock.Config(0, 0),
    minutes = 0,
    system = System.Arena,
    variant = Standard,
    position = Standard.startingPosition,
    openingTable = none,
    mode = draughts.Mode.Rated,
    password = None,
    waitMinutes = 0,
    startDate = none,
    berserkable = true,
    streakable = true,
    teamBattle = none,
    description = none
  )

  private def updateTour(tour: Tournament, data: CrudForm.Data) = {
    import data._
    val clock = draughts.Clock.Config((clockTime * 60).toInt, clockIncrement)
    tour.copy(
      name = name,
      clock = clock,
      minutes = minutes,
      variant = realVariant,
      startsAt = date,
      schedule = Schedule(
        freq = Schedule.Freq.Unique,
        speed = Schedule.Speed.fromClock(clock),
        variant = realVariant,
        position = realVariant.startingPosition,
        at = date
      ).some,
      spotlight = Spotlight(
        headline = headline,
        description = description,
        homepageHours = homepageHours.some.filterNot(0 ==),
        iconFont = none,
        iconImg = image.some.filter(_.nonEmpty)
      ).some,
      position = startingPosition,
      openingTable = openingTable,
      noBerserk = !data.berserkable,
      noStreak = !data.streakable,
      teamBattle = data.teamBattle option (tour.teamBattle | TeamBattle(Set.empty, 10)),
      password = password
    ) |> { tour =>
        tour.perfType.fold(tour) { perfType =>
          tour.copy(conditions = data.conditions.convert(perfType, Map.empty)) // the CRUD form doesn't support team restrictions so Map.empty is fine
        }
      }
  }
}
