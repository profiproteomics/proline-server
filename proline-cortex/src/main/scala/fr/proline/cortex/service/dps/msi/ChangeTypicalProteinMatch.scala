package fr.proline.cortex.service.dps.msi

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION

import com.thetransactioncompany.jsonrpc2.util.NamedParamsRetriever
import com.typesafe.scalalogging.LazyLogging

import fr.profi.util.serialization.ProfiJson
import fr.proline.core.algo.msi.TypicalProteinChooserRule
import fr.proline.core.service.msi.RSMTypicalProteinChooser
import fr.proline.cortex.api.service.dps.msi.IChangeRepresentativeProteinMatchService
import fr.proline.cortex.api.service.dps.msi.RepresentativeProteinMatchRule
import fr.proline.cortex.util.DbConnectionHelper
import fr.proline.jms.service.api.AbstractRemoteProcessingService

/**
 *  Define JMS Service wich allows to select the ProteinSets typical protein using regexp based rules (on protien accession or description). 
 *  List of rules could be specified and will be applied using order priority
 *  
 *  Input params :
 *    project_id : The id of the project used for data importation.
 *    result_summary_id : The id of the Result Summary to change typical protein for.
 *    change_typical_rules :  List of rule_regex (the regular expression used to select typical protein on each Protein Set)
 *             and rule_on_ac (Specify if regular expression should be tested on Accession. If NO will be tested on description.) to use
 *  
 *  Output params : 
 *    Boolean for service run status
 */

class ChangeTypicalProteinMatch extends AbstractRemoteProcessingService with IChangeRepresentativeProteinMatchService with LazyLogging {

  def doProcess(paramsRetriever: NamedParamsRetriever): Any = {

    require((paramsRetriever != null), "no parameter specified")

    val projectId = paramsRetriever.getLong(PROCESS_METHOD.PROJECT_ID_PARAM)
    val resultSummaryId = paramsRetriever.getLong(PROCESS_METHOD.RESULT_SUMMARY_ID_PARAM)

    val msgLogBuilder = new StringBuilder(" Change typical protein using rules : [")
    val allRulesBuilder = Seq.newBuilder[TypicalProteinChooserRule]
    var ruleIndex = 1

    val rulesStr = paramsRetriever.getList(PROCESS_METHOD.CHANGE_TYPICAL_RULES_PARAM).toArray().foreach(entry => {
      
      if (ruleIndex > 1) //For logger only
        msgLogBuilder.append(", ")
        
      // TODO: use the rule instantiated using RepresentativeProteinMatchRule case class
      val reprProtMatchRule = ProfiJson.getObjectMapper().convertValue(entry, classOf[RepresentativeProteinMatchRule])

      val ruleAsMap = ProfiJson.deserialize[Map[String, AnyRef]](ProfiJson.serialize(entry))
      assert(
        reprProtMatchRule.ruleRegex == ruleAsMap("rule_regex"),
        "problem during deserialization of RepresentativeProteinMatchRule"
      )
        
      if (!ruleAsMap.contains("rule_regex") || !ruleAsMap.contains("rule_on_ac"))
        throw new RuntimeException("Missing rule parameter (rule_regex/rule_on_ac) in rule #" + ruleIndex)
      
      val ruleName = "R" + ruleIndex
      allRulesBuilder += new TypicalProteinChooserRule(ruleName, ruleAsMap("rule_on_ac").asInstanceOf[Boolean], ruleAsMap("rule_regex").asInstanceOf[String])
      msgLogBuilder.append("{rule: ").append(ruleName).append(", onAcc: ").append(ruleAsMap("rule_on_ac").asInstanceOf[Boolean]).append(", RegEx: ").append(ruleAsMap("rule_regex").asInstanceOf[String]).append("}")
      ruleIndex += 1
    })

	  msgLogBuilder.append("]")
    logger.info(msgLogBuilder.result)
    
    val execCtx = DbConnectionHelper.createJPAExecutionContext(projectId)  // Use JPA context
    val typProtChooser = new RSMTypicalProteinChooser(execCtx, resultSummaryId, allRulesBuilder.result)

    var result = true
    try {
      typProtChooser.run
    } catch {
      case ex: Exception => {
        result = false
        logger.error("Error running Change TYpicalProtien service ExecutionContext", ex)
      }
    } finally {
      try {
        execCtx.closeAll()
      } catch {
        case exClose: Exception => logger.error("Error closing ExecutionContext", exClose)
      }
    }

    result
  }


}