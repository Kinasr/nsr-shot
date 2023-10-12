package kinasr.nsr_shot.model;

import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;

import java.util.ArrayList;
import java.util.List;

public class ShotAttribute {
    private final List<By> locators = new ArrayList<>();
    private final List<WebElement> elements = new ArrayList<>();
    private String name;

    public ShotAttribute name(String name) {
        this.name = name;
        return this;
    }

    public ShotAttribute addLocator(By by) {
        this.locators.add(by);
        return this;
    }

    public ShotAttribute addElement(WebElement element) {
        this.elements.add(element);
        return this;
    }

    public ShotAttribute addLocators(List<By> locators) {
        this.locators.addAll(locators);
        return this;
    }

    public ShotAttribute addElements(List<WebElement> elements) {
        this.elements.addAll(elements);
        return this;
    }

    public String name() {
        return this.name;
    }

    public List<By> locators() {
        return this.locators;
    }

    public List<WebElement> elements() {
        return this.elements;
    }
}
