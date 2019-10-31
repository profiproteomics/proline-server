package fr.proline.module.exporter.commons.view

import java.text.DecimalFormat

import fr.proline.module.exporter.commons.config.ExportConfigConstant

class SmartDecimalFormat(val decimalFormat: DecimalFormat ) {
  //val pattern: String, val symbols: DecimalFormatSymbols) {
  //private val dcf = new DecimalFormat(pattern, symbols)
  
  //private val decimalSeparator = this.decimalFormat.getDecimalFormatSymbols.getDecimalSeparator()
  
  def this(pattern: String, otherDecimalFormat: DecimalFormat) = {
    this( new DecimalFormat(pattern) )
    
    this.decimalFormat.setDecimalFormatSymbols(otherDecimalFormat.getDecimalFormatSymbols)
  }
  
  def this(pattern: String, otherSmartDecimalFormat: SmartDecimalFormat) = {
    this(pattern, otherSmartDecimalFormat.decimalFormat)
  }
  
  // format the given value depending on the given DecimalFormat
  def format(value: Any): Any = {
    if( value == null || value == "" ) return null
    var( isDouble, isFloat, isInt, isLong ) = (false,false,false,false)
    
    val isNaN = value match {
      case d: Double => isDouble = true; d.isNaN
      case f: Float => isFloat = true; f.isNaN
      case i: Int => isInt = true; i.isNaN
      case l: Long => isLong = true; l.isNaN
      case _ => false
    }
    
    if (isNaN) {
      null
    } else if (!isDouble && !isFloat && !isInt && !isLong) {
      value
    } else {
      // DBO: why ????
      if (this.decimalFormat.getDecimalFormatSymbols.getDecimalSeparator == ExportConfigConstant.DECIMAL_SEPARATOR_DOT) {
        decimalFormat.format(value).toDouble
      } else {
        decimalFormat.format(value)
      }
    }

  }
}