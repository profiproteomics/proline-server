package fr.proline.module.parser.maxquant;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class MapUtils {

    public static <K,V> void insertOrUpdate(Map<K, List<V>> map, K key, V value) {
        if (!map.containsKey(key)) {
            map.put(key, new ArrayList<>());
        }
        map.get(key).add(value);
    }


}
