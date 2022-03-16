package factory;

import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.threading.*;

public class Consumer extends FactoryWorker {
    private NumberQueue queue;
    private final int id;

    public Consumer(NumberQueue queue, int id) {
        super("Consumer", id, queue);
        this.queue = queue;
        this.id = id;
    }

    @Override
    public int action() throws IndexOutOfBoundsException{
        try {
            return queue.dequeue();
        } catch (Exception e) {
            e.printStackTrace();
            throw e;
        }
    }

    @Override
    public void message(int number) {
        System.out.println("Consumer " + id + " picked " + number + " from the belt!");
    }

    @Override
    public void run() {
        while (Thread.currentThread().isInterrupted() == false) {
            try {
                message(action());
            } catch (Exception e) {
                messageError();
            }
        }
    }
}