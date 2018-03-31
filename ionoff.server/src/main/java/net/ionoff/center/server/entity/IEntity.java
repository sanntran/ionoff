package net.ionoff.center.server.entity;

import java.io.Serializable;

public interface IEntity extends Serializable {

	long DEFAULT_ID = 0;
	String ID = "id";
	String NAME = "name";
	
	long getId();
	
	void setId(long id);
	
	String getName();
	
	void setName(String name);
	
	boolean isNew();

}
