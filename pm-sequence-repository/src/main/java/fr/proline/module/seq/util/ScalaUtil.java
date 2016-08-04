package fr.proline.module.seq.util;

import scala.None$;
import scala.Option;

public class ScalaUtil {

	 @SuppressWarnings("unchecked")
	public static <T> Option<T> none() {
	        return (Option<T>) None$.MODULE$;
	    }
	 

}
