package net.ionoff.center.server.entity;

public class BaseObj implements IEntity {

	private static final long serialVersionUID = 1L;

	private long id;
	private String name;

	@Override
	public long getId() {
		return id;
	}
	@Override
	public void setId(long id) {
		this.id = id;
	}

	@Override
	public String getName() {
		return name;
	}
	@Override
	public void setName(String name) {
		this.name = name;
	}

	public String getSId() {
		return "#" + id;
	}

	public String getNameId() {
		return name + " #" + id;
	}
	
	public static boolean isNew(long id) {
		return id == 0;
	}
	
	@Override
	public boolean isNew() {
		return getId() == 0;
	}
	
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		
		builder.append("Id: ").append(getId())
				.append(", Name: " + getName());
		
		return builder.toString();
	}
	
	@Override
	public int hashCode() {
		Long idL = id;
		return idL.hashCode();
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj == null || !(obj instanceof IEntity)) {
			return false;
		}
		IEntity entity = (IEntity) obj;
		return entity.getId() == id;
	}

}
