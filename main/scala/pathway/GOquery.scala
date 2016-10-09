package pathway
import java.sql.ResultSet

/**
  * Created by liuqun on 7/16/16.
  */
class GOquery extends MysqlFunc {
  Host = MysqlAddress.GOHOST
  usr = "go_select"
  pwd = "amigo"
  val GOdescriptQuery =
    """
      SELECT acc, name, term_type
      FROM
      `go_latest`.`term` AS term
      """.stripMargin

  val childQuery =
    """
      SELECT DISTINCT term.acc, term.name, term.term_type, graph_path.distance, descendant.acc
      FROM
      `go_latest`.`term` AS term
      INNER JOIN `go_latest`.`graph_path` AS graph_path ON (term.id=graph_path.term1_id)
      INNER JOIN `go_latest`.`term` AS descendant ON (graph_path.term2_id = descendant.id)
      where graph_path.distance <> 0
    """.stripMargin

  val geneAssociationQuery =
    """
      SELECT
        term.acc,
        gene_product.symbol,
        dbxref.xref_dbname,
        dbxref.xref_key,
        evidence.code
       FROM   `go_latest`.`gene_product` AS gene_product
        INNER JOIN `go_latest`.`dbxref` AS dbxref ON (gene_product.dbxref_id=dbxref.id)
        INNER JOIN `go_latest`.`species` AS species ON (gene_product.species_id=species.id)
        INNER JOIN `go_latest`.`association` AS association ON (gene_product.id=association.gene_product_id)
        INNER JOIN `go_latest`.`evidence` AS evidence ON (association.id=evidence.association_id)
        INNER JOIN `go_latest`.`term` AS term ON (association.term_id=term.id)
      WHERE
        species.common_name='human'
    """.stripMargin
  val geneAssociationQuerynoIEA =
    """
      SELECT
        term.acc,
        gene_product.symbol,
        dbxref.xref_dbname,
        dbxref.xref_key
       FROM   `go_latest`.`gene_product` AS gene_product
        INNER JOIN `go_latest`.`dbxref` AS dbxref ON (gene_product.dbxref_id=dbxref.id)
        INNER JOIN `go_latest`.`species` AS species ON (gene_product.species_id=species.id)
        INNER JOIN `go_latest`.`association` AS association ON (gene_product.id=association.gene_product_id)
        INNER JOIN `go_latest`.`evidence` AS evidence ON (association.id=evidence.association_id)
        INNER JOIN `go_latest`.`term` AS term ON (association.term_id=term.id)
      WHERE
        species.common_name='human'
        AND evidence.code != 'IEA'
    """.stripMargin

  def childresult = this.connect(childQuery)
  def generesultnoIEA = this.connect(geneAssociationQuerynoIEA)
  def generesult = this.connect(geneAssociationQuery)
  def getGOList(result:ResultSet = this.childresult) = {
    val genels = RS2List(result).groupBy(_(0)).map{ case (k,v) => (k, v(0)(1),v(0)(2),v.drop(3))}
    //println(genels.take(5).mkString("\n"))
    genels
  }
}
