import scala.language.postfixOps
import scala.reflect.macros.blackbox



object Sql {

  trait Type

  object Type {

    trait Bool extends Type

    trait Text extends Type

    trait NumberLike extends Type
    trait IntegralLike extends NumberLike

    trait Integral extends IntegralLike

    trait TimestampLike extends Type

    trait Date extends TimestampLike

    trait Timestamp extends TimestampLike

    trait Decimal extends NumberLike

    trait Float extends NumberLike

    trait Tuple extends Type

  }

  trait ~>[A, S <: Type] {
    def encode(v: A): String
  }

  implicit class SymbolOps(symbol: Symbol) {
    def s: String = symbol.toString().substring(1)
  }

  def create[A, S <: Type](fn: A => String):A ~> S = (v:A) => fn(v)
  def quote(s:String) = s"'$s'"

  implicit val StringToText: String ~> Type.Text   = create[String,  Type.Text]    (s => quote(s))
  implicit val IntToIntegral: Int ~> Type.Integral = create[Int,     Type.Integral](i => i.toString)
  implicit val boolean: Boolean ~> Type.Bool       = create[Boolean, Type.Bool](b => if (b) "TRUE" else "FALSE")

  trait Expr[+A <: Type] {
    def plot:String
    def placeholders:Seq[Symbol]  = Seq()
    def bindings:Map[Symbol, Any] = Map()
  }

  object Expr {
    def mergePlaceholders(l:Seq[Seq[Symbol]]):Seq[Symbol]     = l.flatten.toSet.toSeq
    def mergeBindings(l:Seq[Map[Symbol,Any]]):Map[Symbol,Any] = l.foldRight(Map():Map[Symbol,Any]) { _ ++ _ }
  }

  case class LiftedValExpr[+S <: Type](override val plot:String) extends Expr[S]

  case class BinOpExpr[+S <: Type](a: Expr[Type], b:Expr[Type], op:String, wrapOperands:Boolean = true) extends Expr[S] {
    override def plot:String = if (wrapOperands) s"(${a.plot}) $op (${b.plot})" else s"${a.plot} $op ${b.plot}"

    override def placeholders = Expr.mergePlaceholders(Seq(a.placeholders, b.placeholders))
    override def bindings     = Expr.mergeBindings(Seq(a.bindings, b.bindings))
  }

  case class Meta(name:Symbol, defaultAlias: Symbol, schema:Symbol = 'public) {
    def ident:String = if (schema == 'public) name.s else s"${schema.s}.${name.s}"
    def aliased:String = defaultAlias.s
  }

  case class Table(_meta:Meta) {
    def column[A <: Type](col:Symbol):ColumnExpr[A] = ColumnExpr(this, col)
  }

  case class ColumnExpr[+A <: Type](table:Table, col:Symbol) extends Expr[A] {
    override def plot = s"${table._meta.aliased}.${col.s}"
  }

  object tbl_order extends Table(Meta('tbl_order, 'o)) {
    val id = column[Type.Integral]('id)
    val customer_id = column[Type.Integral]('customer_id)
    val status = column[Type.Text]('status)
  }

  object tbl_customer extends Table(Meta('tbl_customer, 'ct)) {
    val id = column[Type.Integral]('id)
    val group_id = column[Type.Integral]('customer_id)
  }

  object tbl_customer_group extends Table(Meta('tbl_customer_group, 'cg)) {
    val id = column[Type.Integral]('id)
    val name = column[Type.Text]('name)
  }

  case class SelectBuilder(
                            cols:Seq[Expr[Type]] = Seq(),
                            from: Option[Table] = None,
                            fromAlias: Option[Symbol] = None,
                            joins:Seq[JoinBuilder] = Seq(),
                            where:Option[Expr[Type.Bool]] = None,
                            groupBy:Seq[Expr[Type]] = Seq(),
                            having:Option[Expr[Type.Bool]] = None,
                            orderBy:Seq[SortExpr] = Seq(),
                            limit: Option[Expr[Type.Integral]] = None,
                            offset: Option[Expr[Type.Integral]] = None,
                          ) {
    def plot:String = {
      val buffer = new StringBuilder("SELECT ")

      // COLS
      buffer.append(
        "\n  " + cols.map(_.plot).mkString("\n, ")
      )

      // FROM
      if (from.isDefined) buffer.append("\nFROM " + from.get._meta.ident)
      if (fromAlias.isDefined) buffer.append(" AS " + fromAlias.get.s)

      // JOINS
      buffer.append(
        "\n" + joins.map(_.plot).mkString("\n")
      )

      // WHERE
      if (where.isDefined) buffer.append(s"\nWHERE ${where.get.plot}")

      buffer.toString()
    }
  }

  case class JoinBuilder(sb: SelectBuilder, joinType:String, table:Table, alias:Option[Symbol] = None, on: Option[Expr[Type.Bool]] = None) {
    def plot:String = {
      val aliasPlot = if(alias.isDefined) s" AS ${alias.get.s}" else ""
      s"${joinType} JOIN ${table._meta.ident}$aliasPlot ON ${on.get.plot}"
    }
  }



  trait SelectExpr extends Expr[Type.Tuple]

  case class SortExpr(e:Expr[Type], direction:String)

  trait CompleteSelect extends SelectExpr {
    protected def sb:SelectBuilder
    override def plot = sb.plot

    protected def toPHSeq[S <: Type](o:Option[Expr[S]]):Seq[Symbol] = o map (_.placeholders) getOrElse { Seq() }
    protected def toPHSeq[S <: Type](seq:Seq[Expr[S]]):Seq[Symbol]  = seq map (_.placeholders) flatten

    override def placeholders: Seq[Symbol] = {
      Expr.mergePlaceholders(
        Seq(
          toPHSeq(sb.cols),
          sb.joins map { j => toPHSeq(j.on) } flatten,
          toPHSeq(sb.where),
          toPHSeq(sb.groupBy),
          toPHSeq(sb.having),
          toPHSeq(sb.orderBy.map(se => se.e)),
          toPHSeq(sb.limit),
          toPHSeq(sb.offset),
        )
      )
    }

    protected def toBDMap(o:Option[Expr[Type]]):Map[Symbol,Any] = o map (_.bindings) getOrElse { Map() }
    protected def toBDMap(seq:Seq[Expr[Type]]):Map[Symbol,Any]  = Expr.mergeBindings(seq map (_.bindings))

    override def bindings: Map[Symbol,Any] = {
      Expr.mergeBindings(
        Seq(
          toBDMap(sb.cols),
          Expr.mergeBindings( sb.joins map { j => toBDMap(j.on) } ),
          toBDMap(sb.where),
          toBDMap(sb.groupBy),
          toBDMap(sb.having),
          toBDMap(sb.orderBy.map(se => se.e)),
          toBDMap(sb.limit),
          toBDMap(sb.offset),
        )
      )
    }

    def missingBindings:Seq[Symbol] = (placeholders.toSet[Symbol] diff bindings.keys.toSet[Symbol]).toSeq
  }

  class SelectInitial(override protected val sb:SelectBuilder) extends CompleteSelect {
    def FROM(t:Table):From = new From(sb.copy(from=Option(t), fromAlias = Option(t._meta.defaultAlias)))
  }

  class From(override protected val sb:SelectBuilder) extends AfterFrom(sb) {
    def AS(alias:Symbol):AfterFrom = new AfterFrom(sb.copy(fromAlias = Option(alias)))
  }

  class AfterFrom(override protected val sb:SelectBuilder) extends AfterJoin(sb) {
    def INNER_JOIN(t:Table) = _JOIN("INNER", t)
    def LEFT_JOIN(t:Table)  = _JOIN("LEFT", t)
    def RIGHT_JOIN(t:Table) = _JOIN("RIGHT", t)
    def FULL_JOIN(t:Table)  = _JOIN("FULL", t)
    protected def _JOIN(joinType:String, table:Table) = InJoin(sb, JoinBuilder(sb, joinType, table, Some(table._meta.defaultAlias)))
  }

  case class InJoin(sb:SelectBuilder, jb:JoinBuilder) {
    def AS(alias:Symbol) = InJoin(sb, jb.copy(alias = Some(alias)))
    def ON(cond:Expr[Type.Bool]):AfterFrom = {
      val join = jb.copy(on = Some(cond))
      new AfterFrom(sb.copy(joins=sb.joins :+ join))
    }
  }

  class AfterJoin(override protected val sb:SelectBuilder) extends AfterWhere(sb) {
    def WHERE(cond:Expr[Type.Bool]):AfterWhere = new AfterWhere(sb.copy(where=Some(cond)))
  }

  class AfterWhere(override protected val sb:SelectBuilder) extends AfterGroupBy(sb) {
    def GROUP_BY(cols:Expr[Type]*):AfterGroupBy = new AfterGroupBy(sb.copy(groupBy=cols))
  }

  class AfterGroupBy(override protected val sb:SelectBuilder) extends AfterHaving(sb) {
    def HAVING(cond:Expr[Type.Bool]):AfterHaving = new AfterHaving(sb.copy(having=Some(cond)))
  }

  class AfterHaving(override protected val sb:SelectBuilder) extends AfterOrderBy(sb) {
    def ORDER_BY(sorts:SortExpr*):AfterOrderBy = new AfterGroupBy(sb.copy(orderBy=sorts))
  }

  class AfterOrderBy(override protected val sb:SelectBuilder) extends AfterLimit(sb) {
    def LIMIT(limit:Expr[Type.Integral]):AfterLimit = new AfterLimit(sb.copy(limit=Some(limit)))
  }

  class AfterLimit(override protected val sb:SelectBuilder) extends AfterOffset(sb) {
    def OFFSET(offset:Expr[Type.Integral]):AfterOffset = new AfterOffset(sb.copy(offset=Some(offset)))
  }

  class AfterOffset(override protected val sb:SelectBuilder) extends CompleteSelect {
  }


  object SELECT {
    def apply(cols:Expr[Type]*):SelectInitial = new SelectInitial(SelectBuilder(cols))
  }

  //**** SYNTEX CLASSES
  implicit class AnyExprSyntax(a:Expr[Type]) {
    def ===(b:Expr[Type]):Expr[Type.Bool] = BinOpExpr[Type.Bool](a,b,"=",false)
    def !==(b:Expr[Type]):Expr[Type.Bool] = BinOpExpr[Type.Bool](a,b,"!=",false)

  }

  implicit class SameTypeExprSyntax[S <: Type](a:Expr[S]) {
    def IN(opts:Expr[S]*):Expr[Type.Bool] = new Expr[Type.Bool] {
      override def plot:String = s"${a.plot} IN (" + opts.map(_.plot).mkString(",") + ")"

      override def placeholders: Seq[Symbol] = Expr.mergePlaceholders(opts.map(_.placeholders))
      override def bindings: Map[Symbol,Any] = Expr.mergeBindings(opts.map(_.bindings))
    }
  }

  implicit class MathOps[A <: Type.NumberLike](a:Expr[A]) {
    def +(b:Expr[Type.NumberLike]):Expr[Type.NumberLike] = BinOpExpr[Type.NumberLike](a,b,"+",false)
    def -(b:Expr[Type.NumberLike]):Expr[Type.NumberLike] = BinOpExpr[Type.NumberLike](a,b,"-",false)
    def *(b:Expr[Type.NumberLike]):Expr[Type.NumberLike] = BinOpExpr[Type.NumberLike](a,b,"*",false)
    def /(b:Expr[Type.NumberLike]):Expr[Type.NumberLike] = BinOpExpr[Type.NumberLike](a,b,"/",false)
  }

  implicit class BoolOps(a:Expr[Type.Bool]) {
    def AND(b:Expr[Type.Bool]):Expr[Type.Bool] = BinOpExpr[Type.Bool](a,b,"AND")
    def OR(b:Expr[Type.Bool]):Expr[Type.Bool]  = BinOpExpr[Type.Bool](a,b,"OR")
  }


  implicit def LiftOps[S <: Type, A](v:A)(implicit converter:A ~> S):Expr[S] = LiftedValExpr[S](converter.encode(v))


  implicit class SortExprOps[A <: Type](e:Expr[A]) {
    def ASC: SortExpr = new SortExpr(e, "ASC")
    def DESC: SortExpr = new SortExpr(e, "DECS")
  }
  //****


  //************* helper methods
  def bind[S <: Type](placeholder:Symbol):Expr[S] = new Expr[S] {
    override def plot = s":${placeholder.s}"
    override def placeholders = Seq(placeholder)
  }

  def bind[S <: Type](placeholder:Symbol, v:Any):Expr[S] = new Expr[S] {
    override def plot = s":${placeholder.s}"
    override def placeholders = Seq(placeholder)
    override def bindings = Map(placeholder -> v)
  }

  def plain[S <: Type](expr:String):Expr[S] = new Expr[S] {
    override def plot = expr
  }

  //def fn[S <: Type](fn:String)(args:Expr[Type]*):Expr[S] = new Expr[S] {}
  //**************

  // binds
  val x = "status"

  // aliases
  val (o,ct,cg) = (tbl_order,tbl_customer,tbl_customer_group)

  val query = (
    SELECT (
      o.id
      , o.status
    )
      FROM  tbl_order AS 'o
      INNER_JOIN tbl_customer AS 'ct       ON (o.customer_id === ct.id)
      LEFT_JOIN  tbl_customer_group AS 'cg ON (ct.group_id   === cg.id)
      WHERE (
              (ct.id === bind[Type.Integral]('customerId))
               AND (o.status IN ("intransit", bind[Type.Text]('st2, x)))
           )

      /* NOT ( ct.id > bind[Int]('customerId)
        AND (o.status IN ("intransit", "delivered"))
        AND (o.id BETWEEN bind[Int]('start) AND bind[Int]('end))
        AND (CASE
        WHEN TRUE  THEN 4
        WHEN FALSE THEN 7
        ELSE 5
        END
        )
        AND ( ct.id IN (SELECT (o.id) FROM tbl_customer) )
      )*/
      GROUP_BY o.id
      ORDER_BY (o.id ASC, cg.id + 2 DESC)
    )


  query.plot
  query.placeholders
  query.bindings
  query.missingBindings
}
