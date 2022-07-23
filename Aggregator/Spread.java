package Aggregator;

import java.util.Set;

import org.zeromq.ZMQ;

public class Spread implements Runnable {
    private ZMQ.Socket pub;
    private ZMQ.Socket pullAggregator;
    private ZMQ.Socket pushAggregator;
    private Set<Integer> neighbours;
    private CRDT crdt;
    
    public Spread(ZMQ.Socket pub, ZMQ.Socket pullAggregator,ZMQ.Socket pushAggregator, Set<Integer> neighbours, CRDT crdt) {
        this.pub = pub;
        this.pullAggregator = pullAggregator;
        this.pushAggregator = pushAggregator;
        this.neighbours = neighbours;
        this.crdt = crdt;
    }

    public void run() {
        while(true) {
            byte[] msg = pullAggregator.recv();

            if(crdt.deserialize(new String(msg))) {
                for(Integer i : neighbours) {
                    pushAggregator.send(msg);
                }
            }

            for (String percent : crdt.validatePercentage())
                pub.send(percent);
        }
    }
}
