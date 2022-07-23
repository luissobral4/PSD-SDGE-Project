package Client;

import java.util.List;
import java.util.Map;
import java.util.Scanner;

import org.zeromq.ZMQ;

public class Controller {
    // classe that saves the state of active notifications
    private Notification notification;
    // scanner to read input
    private Scanner s;
    // socket request/reply
    private ZMQ.Socket req;
    // socket publish/subscribe
    private ZMQ.Socket sub;

    public Controller(ZMQ.Socket req, ZMQ.Socket sub, Map<String, List<String>> types) {
        this.s = new Scanner(System.in);
        this.notification = new Notification(types);
        this.req = req;
        this.sub = sub;
    }

    // read int between min and max
    private int lerInt(int min, int max){
        int n = -1;
        String str;
        if(max  < 100)
            str = "Insira uma opção:";
        else
            str = "Insira um valor de 0 a 100:";

        do{
            System.out.println(str);
            try {
                String line = s.nextLine();
                n = Integer.parseInt(line);
            } catch (NumberFormatException nfe) {
                System.out.println("Opção inválida");
                n = -1;
            }
        } while (n < min || n > max);

        return n;
    }

    // notification1 state handler
    private void notification1(int command){
        int op;
        String type;
        boolean state;
        List<String> not = notification.getTypes(command);
        View.printMenu(not, "Notifications (Type State)");

        op = lerInt(0, not.size());
        if (op > 0) {
            type = notification.getType(op - 1);
            state = notification.setOpposite(command, type);
            if(state)
                this.sub.subscribe("offline-"+type);
            else
                this.sub.unsubscribe("offline-"+type);
            notification1(command);
        }
    }

    // notification2 state handler
    private void notification2(int command){
        int op;
        String type;
        boolean state;

        state = notification.isNot2All();
        List<String> not = notification.getTypes(command);
        not.add("Todos "+state);
        View.printMenu(not, "Notifications (Type State)");

        op = lerInt(0, not.size());

        if(op == not.size()){
            if(state){
                this.sub.unsubscribe("record-");
                notification.setNot2All(false);
            } else {
                this.sub.subscribe("record-");
                notification.setNot2All(true);
            }
            notification2(command);
        }else if (op > 0) {
            type = notification.getType(op - 1);
            state = notification.setOpposite(command, type);
            if(state)
                this.sub.subscribe("record-"+type);
            else
                this.sub.unsubscribe("record-"+type);
            notification2(command);
        }
    }

    // notification 3 and 4 state handler
    private void notification34(int command){
        int op,value;
        List<String> values = notification.getXUsedList(command);
        values.add("Adiconar X%");
        View.printMenu(values, "Notificações (X%)");
        op = lerInt(0, values.size());

        if(op >0) {
            if(op == values.size()){
                values = notification.getXUnusedList(command);
                int size = values.size();
                View.printMenu(values, "Escolher % (X%)");
                op = lerInt(0, size);
                if(op > 0) {
                    value = Integer.valueOf(values.get(op-1));
                    if(!notification.containsX(command, value)){
                        notification.setX(command, value);
                        if(command == 3)
                            this.sub.subscribe("percentUp-"+value);
                        else
                            this.sub.subscribe("percentDown-"+value);
                    }
                }
            } else {
                value = notification.getX(command,op-1);
                if(notification.containsX(command, value)){
                    notification.removeX(command, value);
                    if(command == 3)
                        this.sub.unsubscribe("percentUp-"+value);
                    else
                        this.sub.unsubscribe("percentDown-"+value);
                }
            }
            notification34(command);
        }
    }

     // request 1 and 4 handler
    private void request14(int command){
        int op;
        List<String> values;
        if(command == 1) 
            values = notification.getTypes();
        else
            values = notification.getEvents();

        View.printMenu(values, "Pedido (Type)");
        op = lerInt(0, values.size());

        if(op > 0){
            if(command == 1){
                String type = notification.getType(op - 1);
                req.send("1,"+ type);
                String res = req.recvStr();
                System.out.println("Número de dispositivos do tipo " + type + " online: " + res);
            }else if(command == 4){
                String event = notification.getEvent(op - 1);
                req.send("4,"+ event);
                String res = req.recvStr();
                System.out.println("Número de eventos do tipo " + event + ": " + res);
            }
        }
    }

    // request 2 handler
    private void request2(){
        System.out.println("Insira um ID de dispositivo:");
        String id = s.nextLine();

        req.send("2,"+ id);
        if(Boolean.parseBoolean(req.recvStr()))
            System.out.println("O dispositivo " + id + " está online no sistema");
        else
            System.out.println("O dispositivo " + id + " não está online no sistema");
    }

    // request 3 handler
    private void request3(){
        req.send("3");
        String res = req.recvStr();
        System.out.println("Número de dispositivos ativos no sistema: " + res);
    }

    // controller for request and notifications
    private void controller2(int type) {
        int command;
        boolean on = true;

        while (on) {
            if(type == 2)
                View.printRequestsMenu();
            else
                View.printNotificationsMenu();
                
            command = lerInt(0, 4);

            switch (command) {
                case 1:
                    if(type == 2)
                        request14(command);
                    else
                        notification1(command);
                    break;
                case 2:
                    if(type == 2)
                        request2();
                    else
                        notification2(command);
                    break;
                case 3:
                    if(type == 2)
                        request3();
                    else
                        notification34(command);
                    break;
                case 4:
                    if(type == 2)
                        request14(command);
                    else
                        notification34(command);
                    break;
                case 0:
                    on = false;
                    break;
                default:
                    break;
            }
        }
    }

    // main controller
    public void controller() {
        int command;
        boolean on = true;

        while (on) {
            View.printMainMenu();
            command = lerInt(0, 2);

            switch (command) {
                case 1:
                    controller2(command);
                    break;
                case 2:
                    controller2(command);
                    break;
                case 0:
                    on=false;
                    break;
                default:
                    break;
            }
        }
    }
}
