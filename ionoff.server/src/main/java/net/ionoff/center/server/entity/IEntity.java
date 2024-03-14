package net.ionoff.center.server.entity;

import java.io.Serializable;

public interface IEntity extends Serializable {

	String ID = "id";
	String NAME = "name";
	
	long getId();
	
	void setId(long id);
	
	String getName();
	
	void setName(String name);

	default String getSId() {
		return "#" + getId();
	}

	default String getNameId() {
		return getName() + " #" + getId();
	}

	static boolean isNew(long id) {
		return id == 0;
	}

	default boolean isNew() {
		return getId() == 0;
	}

}
