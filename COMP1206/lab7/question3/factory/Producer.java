package factory;

import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.threading.*;
import java.util.concurrent.ThreadLocalRandom;

public class Producer extends FactoryWorker {
    NumberQueue queue;
    int id;

    public Producer(NumberQueue queue, int id) {
        super("Producer", id, queue);
    }

    @Override
    public int action() throws IndexOutOfBoundsException{
        int number = ThreadLocalRandom.current().nextInt();
        try {
            queue.enqueue(number);
            return number;
        } catch (Exception e) {
            throw e;
        }
    }

    @Override
    public void message(int number) {
        System.out.println("Producer " + id + " put " + number + " on the belt!");
    }

    @Override
    public void run() {
        while (Thread.currentThread().isInterrupted() == false) {
            try {
                message(action());
            } catch (Exception e) {
                e.printStackTrace();
                messageError();
            }
        }
    }
}