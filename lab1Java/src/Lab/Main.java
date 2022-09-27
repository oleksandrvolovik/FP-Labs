package Lab;

import java.util.*;

public class Main {

    public static void main(String[] args) {
        System.out.println(Singleton("test"));
        System.out.println(Null(List.of("not null")));
        System.out.println(snoc(Collections.singletonList("first"), "second"));
        System.out.println(length(List.of("a", "b", "c")));
    }

    static List Singleton(Object arg) {
        return List.of(arg);
    }

    static boolean Null(List list) {
        return list.size() == 0;
    }

    static List<Object> snoc(List<Object> list, Object element) {
        List<Object> result = new ArrayList<>(list);
        result.add(element);
        return result;
    }

    static Integer length(List list) {
        Integer result = 0;
        for (Object element : list) {
            result++;
        }
        return result;
    }

}
