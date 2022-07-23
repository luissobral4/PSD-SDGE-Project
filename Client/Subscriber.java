package Client;

import org.zeromq.ZMQ;
import org.zeromq.ZMQException;

// this class reads and print all subscribed info
public class Subscriber implements Runnable{
    private ZMQ.Socket sub;

    public Subscriber(ZMQ.Socket sub) {
        this.sub = sub;
    }

    @Override
    public void run() {
        try {
                while(true) {
                byte[] msg = this.sub.recv();
                System.out.println("\nNotificação: " + new String(msg));
            }   
        } catch (ZMQException ignore) {}
    }
}