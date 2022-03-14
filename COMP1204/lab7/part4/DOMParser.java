import javax.xml.parsers.*;
import org.w3c.dom.*;

import java.util.*;
import java.io.*;

public class DOMParser {

  private ArrayList<Pizza> pizzas = new ArrayList<>();
  private Pizza pizza;

  public DOMParser(String filename) {
    try {
      File input = new File(filename);
      DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
      factory.setNamespaceAware(true);

      DocumentBuilder builder = factory.newDocumentBuilder();
      Document doc = builder.parse(input);
      doc.getDocumentElement().normalize();

      var children =
          doc.getDocumentElement().getElementsByTagNameNS("http://pizza.ecs.soton.ac.uk", "Pizza");
      for (int i = 0; i < children.getLength(); i++) {
        Element pizza = (Element) children.item(i);
        handlePizza(pizza);
      }
    } catch (Exception e) {
      e.printStackTrace();
    }

    for (Pizza pizza : pizzas) {
      System.out.println(pizza);
    }
  }

  private void handlePizza(Element element) {
    pizza = new Pizza();
    pizzas.add(pizza);

    pizza.setName(cleanPizzaName(element.getAttribute("name")));

    var cheese = element.getElementsByTagNameNS("http://pizza.ecs.soton.ac.uk", "Cheese").item(0);
    handleCheese(pizza, (Element) cheese);

    var base = element.getElementsByTagNameNS("http://pizza.ecs.soton.ac.uk", "Base").item(0);
    handleBase(pizza, (Element) base);

    var toppings = element.getElementsByTagNameNS("http://pizza.ecs.soton.ac.uk", "Topping");
    handleToppings(toppings);
  }

  private String cleanPizzaName(String text) {
    text = text.trim();
    text = text.replaceAll("\"", "");
    if (text.length() == 0) text = "Unknown";
    return text;
  }

  private void handleCheese(Pizza pizza, Element cheese) {
    var cheeseText = cheese.getTextContent();
    cheeseText = cleanText(cheeseText);
    pizza.setCheese(cheeseText);
  }

  private void handleBase(Pizza pizza, Element cheese) {
    var baseText = cheese.getTextContent();
    baseText = cleanText(baseText);
    pizza.setBase(baseText);
  }

  private void handleToppings(NodeList toppings) {
    for (int i = 0; i < toppings.getLength(); i++) {
      Element topping = (Element) toppings.item(i);
      var toppingText = cleanText(topping.getTextContent());
      if (toppingText != "None") pizza.addTopping(toppingText);
    }
  }

  private String cleanText(String text) {
    text = text.trim();
    text = text.replaceAll("\"", "");
    text = text.replace("\t", "");
    if (text.length() == 0) text = "None";
    return text;
  }

  public static void main(String[] args) throws Exception {
    var parser = new DOMParser("pizzas.xml");
  }
}

class Pizza {
  private String cheese;
  private String name;
  private String base;
  private ArrayList<Topping> toppings = new ArrayList<Topping>();

  public void setCheese(String cheese) {
    this.cheese = cheese;
  }

  public void setName(String name) {
    this.name = name;
  }

  public void setBase(String base) {
    this.base = base;
  }

  public void addTopping(String name) {
    toppings.add(new Topping(name));
  }

  @Override
  public String toString() {
    String name = "Pizza name: " + this.name + "\n";
    String base = "Pizza base: " + this.base + "\n";
    String cheese = "Pizza cheese: " + this.cheese + "\n";
    String baseOutput = name + base + cheese + "Pizza toppings: ";

    if (toppings.size() == 0) return baseOutput + "None\n";
    else {
      for (Topping topping : toppings) baseOutput += topping.getName() + ", ";
      baseOutput = baseOutput.substring(0, baseOutput.length() - 2);
      return baseOutput + "\n";
    }
  }
}

class Topping {
  private String name;

  public Topping(String name) {
    this.name = name;
  }

  public String getName() {
    return name;
  }
}
