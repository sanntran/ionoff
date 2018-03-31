package net.xapxinh.center.shared.dto;

public class Command extends SerializableDto {

	private static final long serialVersionUID = 1L;

	private String command;
	private String id;
	private String val;
	private String input;
	private String input_type;
	private String type;
	private String title;
	
	public String getCommand() {
		return command;
	}
	public Command setCommand(String command) {
		this.command = command;
		return this;
	}
	public String getVal() {
		return val;
	}
	public Command setVal(String value) {
		this.val = value;
		return this;
	}
	public String getInput() {
		return input;
	}
	public Command setInput(String input) {
		this.input = input;
		return this;
	}
	public String getType() {
		return type;
	}
	public Command setType(String type) {
		this.type = type;
		return this;
	}
	public String getId() {
		return id;
	}
	public Command setId(String id) {
		this.id = id;
		return this;
	}
	public String getInput_type() {
		return input_type;
	}
	public Command setInput_type(String input_type) {
		this.input_type = input_type;
		return this;
	}
	public String getTitle() {
		return title;
	}
	public Command setTitle(String title) {
		this.title = title;
		return this;
	}
}
