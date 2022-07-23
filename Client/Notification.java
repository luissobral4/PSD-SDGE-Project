package Client;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
//this classe saves the state of the active notifications
public class Notification {
    //map <type,list events> 
    private Map<String, List<String>> types;
    //map <type, state> notification1
    private Map<String, Boolean> notification1;
    //map <type, state> notification2
    private Map<String, Boolean> notification2;
    //notification2 all types active
    private boolean notification2All;
    //set <active percent> notification3
    private Set<Integer> notification3;
    //set <active percent> notification4
    private Set<Integer> notification4;

    public Notification(Map<String, List<String>> types) {
        this.types = types;

        notification1 = new HashMap<>();
        notification2 = new HashMap<>();

        //initialize all types with state false
        for(String t : types.keySet()) {
            notification1.put(t, false);
            notification2.put(t, false);
        }

        notification2All = false;

        notification3 = new TreeSet<>();
        notification4 = new TreeSet<>();
    }

    // return for a given notification(1 or 2) all types and it state
    public List<String> getTypes(int noti) {
        List<String> res = new ArrayList<>();

        if (noti == 1) {
            for (String s : types.keySet())
                res.add(s+" "+notification1.get(s));
        } else {
            for (String s : types.keySet())
                res.add(s+" "+notification2.get(s));
        }
        return res;
    }

    // return for notification 3 or 4, a list of active percents
    public List<String> getXUsedList(int noti) {
        Set<Integer> l;
        if (noti == 3) 
            l = notification3;
        else 
            l = notification4;

        List<String> res = new ArrayList<>();

        for (Integer i : l)
            res.add("Desativar " + i + "%");

        return res;
    }

    // return for notification 3 or 4, a list of innactive percents
    public List<String> getXUnusedList(int command){
        Set<Integer> l;
        List<String> values = new ArrayList<>();
        values.add("10");values.add("20");values.add("30");
        values.add("40");values.add("50");values.add("60");
        values.add("70");values.add("80");values.add("90");

        if(command == 3)
            l = notification3;
        else
            l = notification4;

        for (Integer i : l) 
            values.remove(""+i);

        return values;
    }

    // return for notification 3 or 4, return percent (int) on the given index
    public int getX(int command,int index){
        Set<Integer> l;
        if (command == 3) 
            l = notification3;
        else 
            l = notification4;

        int res = -1;
        for (Integer i : l) {
            if(index == 0){
                res = i;
                break;
            }
            index--;
        }

        return res;
    }

    // For notification 1 or 2, change the state of a given type to the opposite and return it
    public boolean setOpposite(int noti, String type) {
        boolean r;
        if (noti == 1){
            r = !notification1.get(type);
            notification1.put(type,r);
        }
        else{
            r = !notification2.get(type);
            notification2.put(type,r);
        }
        return r;
    }

    // For notification 3 or 4, add percent value v
    public void setX(int noti, int v) {
        if (noti == 3)
            notification3.add(v);
        else
            notification4.add(v);
    }

    // For notification 3 or 4, returns true if value v is on the Set<int>
    public boolean containsX(int noti, int v) {
        if (noti == 3)
            return notification3.contains(v);
        else
            return notification4.contains(v);
    }

    // For notification 3 or 4, remove value v
    public void removeX(int noti, int v) {
        if (noti == 3){
            if (notification3.contains(v))
                notification3.remove(v);
        } else if (notification4.contains(v))
                notification4.remove(v);
    }

    // return the type corresponding to a given id
    public String getType(int id) {
        return (new ArrayList<>(types.keySet())).get(id);
    }

    // return the event corresponding to a given id
    public String getEvent(int id) {
        List<String> events = new ArrayList<>();

        for(List<String> e : types.values()) {
            events.addAll(e);
        }

        return events.get(id);
    }

    // return a list with all types
    public List<String> getTypes() {
        return new ArrayList<>(types.keySet());
    }

    // return a list with all events
    public List<String> getEvents() {
        List<String> events = new ArrayList<>();

        for(List<String> e : types.values()) {
            events.addAll(e);
        }

        return events;
    }

    // return isNot2All state
    public boolean isNot2All() {
        return notification2All;
    }

    // change notification2 state
    public void setNot2All(boolean not2All) {
        this.notification2All = not2All;
    }
}