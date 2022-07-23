package Client;

import java.util.ArrayList;
import java.util.List;

public class View {
    private static void clearPage() {
        for(int i = 0; i<5; i++)
            System.out.println("");
    }

    public static void printLine(int size) {
        for(int i=0; i<size; i++)
            System.out.print("-");

        System.out.println("");
    }

    // print menu with the title message and the given list of strings
    public static void printMenu(List<String> menu, String message){
        clearPage();

        int size = 0, length=message.length();

        for(String linha: menu){
            if(linha.length() + 4 > length)
                length = linha.length() + 4;
        }

        if(length < 20)
            length = 20;

        printLine(length);
        System.out.println(message);
        printLine(length);

        size = menu.size();
        for(int i = 0;i < size;i++)
            System.out.println(i+1+" | "+menu.get(i));

        System.out.println("0 | Sair");
    }

    public static void printNotificationsMenu() {
        List<String> l = new ArrayList<>();
        l.add("Notificações 'Sem Dispositivos Online'");
        l.add("Notificações 'Record de Dispositivos Online'");
        l.add("Notificações 'Subida de Dispositivos Online");
        l.add("Notificações 'Descida de Dispositivos Online");
        printMenu(l,"NOTIFICAÇÕES");
    }

    public static void printMainMenu(){
        List<String> l = new ArrayList<>();
        l.add("Notificações");
        l.add("Pedidos");
        printMenu(l,"MENU");
    }

    public static void printRequestsMenu(){
        List<String> l = new ArrayList<>();
        l.add("Número de dispositivos de um dado tipo online no sistema");
        l.add("Verificar se um dispositivo está online no sistema");
        l.add("Número de dispositivos ativos no sistema");
        l.add("Número de eventos de um dado tipo ocorridos no sistema");
        printMenu(l,"PEDIDOS");
    }
}
