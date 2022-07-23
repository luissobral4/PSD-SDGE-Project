package Aggregator;

import org.zeromq.ZMQ;

public class Reply implements Runnable {
    private ZMQ.Socket rep;
    private CRDT crdt;

    public Reply(ZMQ.Socket rep, CRDT crdt) {
        this.rep = rep;
        this.crdt = crdt;
    }

    private void interpreter(String msg) {
        String res = null;
        String[] args = msg.split(",");

        if(args[0].equals("1")) {
            res = crdt.devicesOnline(args[1]);
        }
        else if(args[0].equals("2")) {
            res = crdt.deviceOnline(args[1]);
        }
        else if(args[0].equals("3")) {
            res = crdt.devicesActive();
        }
        else if(args[0].equals("4")) {
            res = crdt.events(args[1]);
        }

        rep.send(res);
    }
    
    public void run() {
        while(true) {
            byte[] msg = rep.recv();
            interpreter(new String(msg));
        }
    }
}
