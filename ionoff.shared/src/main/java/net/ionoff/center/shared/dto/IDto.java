package net.ionoff.center.shared.dto;

import java.io.Serializable;

public interface IDto extends Serializable {

	long DEFAULT_ID = 0;
	String ID = "id";
	String NAME = "name";
	
	long getId();
	
	void setId(long id);
	
	String getName();
	
	void setName(String name);
	
	String formatNameID();
	
	boolean izNew();

}
