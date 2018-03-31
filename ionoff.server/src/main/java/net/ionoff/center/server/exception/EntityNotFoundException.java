package net.ionoff.center.server.exception;

public class EntityNotFoundException extends RuntimeException {

	private static final long serialVersionUID = 1L;
	
	private Long resourceId;
	private String resourceUniqueName;
	private String resourceName;

	public EntityNotFoundException(String message) {
		super(message);
	}
	
	public EntityNotFoundException(Long resourceId, String resourceName) {
		super(resourceName + " id " + resourceId + " is not found");
		this.resourceId = resourceId;
		this.resourceName = resourceName;
	}
	
	public EntityNotFoundException(String resourceUniqueName, String resourceName) {
		super(resourceName + " name " + resourceUniqueName + " is not found");
		this.resourceUniqueName = resourceUniqueName;
		this.resourceName = resourceName;
	}

	public Long getResourceId() {
		return resourceId;
	}

	public String getResourceName() {
		return resourceName;
	}

	public String getResourceUniqueName() {
		return resourceUniqueName;
	}
}
