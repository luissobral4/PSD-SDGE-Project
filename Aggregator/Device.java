package Aggregator;

import java.util.HashMap;
import java.util.Map;

public class Device {
    private String id;
    private String type;
    private boolean online;
    private boolean active;
    private Map<String, Integer> events;

    public Device(String id, String type, boolean online, boolean active) {
        this.id = id;
        this.type = type;
        this.online = online;
        this.active = active;
        this.events = new HashMap<>();
    }
    
    public Device(String serialized){
        this.deserialize(serialized);
    }

    public String getId() {
        return this.id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getType() {
        return this.type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public boolean isOnline() {
        return this.online;
    }

    public void setOnline(boolean online) {
        this.online = online;
    }

    public boolean isActive() {
        return this.active;
    }

    public void setActive(boolean active) {
        this.active = active;
    }

    public void addEvent(String event) {
        if(events.containsKey(event)) {
            int counter = events.get(event);
            counter++;
            events.replace(event, counter);
        } else {
            events.put(event, 1);
        }
    }

    public int getNumEvents(String event) {
        if(events.containsKey(event)) {
            return events.get(event);
        } else
            return 0;
    }

    public String serialize() {
        //id:type:online:active:event1_int<event2_int<event3_int
        StringBuilder sb = new StringBuilder();
        sb.append(id)
          .append(":")
          .append(type)
          .append(":")
          .append(String.valueOf(online))
          .append(":")
          .append(String.valueOf(active))
          .append(":");

        for(Map.Entry<String, Integer> e : this.events.entrySet()) {
            sb.append(e.getKey()).append("%").append(String.valueOf(e.getValue())).append("!");
        }

        String s = sb.toString();
        return s.substring(0, s.length() - 1);
    }

    private void deserialize(String serialized){
        String[] args = serialized.split(":");

        if(args.length != 5)
            return;

        this.id = args[0];
        this.type = args[1];
        this.online = Boolean.parseBoolean(args[2]);
        this.active = Boolean.parseBoolean(args[3]);

        this.events = new HashMap<>();
        String[] tokens = args[4].split("!");
        for(String t : tokens) {
            String[] entry = t.split("%");
            this.events.put(entry[0],Integer.parseInt(entry[1]));
        }
    }
}
