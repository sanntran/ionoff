package net.ionoff.center.server.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class Condition implements IEntity {

    @EqualsAndHashCode.Include
    private long id;
    private String name;
    private String expression;
    private Mode mode;

    @Override
    public String toString() {
        return "Id: " + getId() + ", Name: " + getName();
    }
}
