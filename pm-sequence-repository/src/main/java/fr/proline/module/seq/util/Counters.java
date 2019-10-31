package fr.proline.module.seq.util;

import org.slf4j.Logger;

import java.util.HashMap;

public class Counters {

  private HashMap<String, int[]> intCounters = new HashMap<String, int[]>();
  private String name;

  public Counters(String name) {
    this.name = name;
  }

  public void inc(String a) {
    int[] valueWrapper = intCounters.get(a);

    if (valueWrapper == null) {
      intCounters.put(a, new int[] { 1 });
    } else {
      valueWrapper[0]++;
    }
  }

  public void append(String a, int v) {
    int[] valueWrapper = intCounters.get(a);

    if (valueWrapper == null) {
      intCounters.put(a, new int[] { v });
    } else {
      valueWrapper[0] += v;
    }
  }

  public void dec(String a) {
    int[] valueWrapper = intCounters.get(a);

    if (valueWrapper == null) {
      intCounters.put(a, new int[] { 1 });
    } else {
      valueWrapper[0]--;
    }
  }

  public int get(String a) {
    int[] valueWrapper = intCounters.get(a);

    if (valueWrapper == null) {
      return 0;
    } else {
      return valueWrapper[0];
    }
  }

  public void initialize() {
    for(String counterName : intCounters.keySet()) {
      int[] valueWrapper = intCounters.get(counterName);
      if (valueWrapper != null) {
        valueWrapper[0] = 0;
      }
    }
  }

  public void report(Logger log) {
    log.info("Counters {}:", name);
    for(String counterName : intCounters.keySet()) {
      log.info("{}.{}: {}", name, counterName, get(counterName));
    }

  }

  /**
   * Sum values of counters containing the specified keyword
   *
   * @param keyword
   * @return
   */
  public int sum(String keyword) {
    int count = 0;
    for(String counterName : intCounters.keySet()) {
      count += counterName.contains(keyword) ? get(counterName) : 0;
    }
    return count;
  }
}

