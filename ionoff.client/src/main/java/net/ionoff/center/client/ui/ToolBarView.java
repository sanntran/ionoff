package net.ionoff.center.client.ui;

import java.util.Date;

import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.InlineLabel;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.TextBox;
import com.google.gwt.user.datepicker.client.CalendarUtil;

import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.constants.WavesType;
import gwt.material.design.client.ui.MaterialButton;
import gwt.material.design.client.ui.MaterialDatePicker;
import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialLink;
import gwt.material.design.client.ui.MaterialListBox;

public class ToolBarView extends FlowPanel {
	
	private final TextBox textBoxKeyWord;

	private final FlowPanel panelDateInput;
	private final MaterialDatePicker dateBoxFrom;
	private final MaterialDatePicker dateBoxTo;


	private final MaterialListBox listBoxSearchBy;
	private final MaterialButton btnSearch;
	private final FlowPanel searchPanel;

	private final MaterialButton btnRefresh;
	private final MaterialButton btnAdd;
	private final MaterialButton btnRemove;
	private final MaterialIcon btnCloseSearch;
	private final MaterialLink lblTitle;
	private final DateTimeFormat dateFormat;

	private final FlowPanel buttonsPanel;
	
	private int days = 1;

	public ToolBarView() {
		setStyleName("toolbar");
		
		lblTitle = new MaterialLink();
		lblTitle.addStyleName("title");
		add(lblTitle);
		
		buttonsPanel = new FlowPanel();
		buttonsPanel.setStyleName("buttons");
		add(buttonsPanel);
		
		btnAdd = new MaterialButton();
		btnAdd.setIconType(IconType.ADD);
		btnAdd.setWaves(WavesType.LIGHT);
		buttonsPanel.add(btnAdd);
		
		btnRefresh = new MaterialButton();
		btnRefresh.setIconType(IconType.REFRESH);
		btnRefresh.setWaves(WavesType.LIGHT);
		buttonsPanel.add(btnRefresh);

		btnRemove = new MaterialButton();
		btnRemove.setIconType(IconType.DELETE_FOREVER);
		btnRemove.setWaves(WavesType.LIGHT);
		buttonsPanel.add(btnRemove);

		searchPanel = new FlowPanel();
		searchPanel.setVisible(false);
		searchPanel.setStyleName("searchbar");
		add(searchPanel);
		
		listBoxSearchBy = new MaterialListBox();
		searchPanel.add(listBoxSearchBy);

		panelDateInput = new FlowPanel();
		panelDateInput.addStyleName("panelDate");
		searchPanel.add(panelDateInput);

		textBoxKeyWord = new TextBox();
		searchPanel.add(textBoxKeyWord);

		String format = "yyyy/mm/dd";
		dateFormat = DateTimeFormat.getFormat("yyyy/MM/dd");
		Date fromDate = new Date();
        CalendarUtil.addDaysToDate(fromDate, -days);

		dateBoxFrom = new MaterialDatePicker();
		dateBoxFrom.addStyleName("date");
		dateBoxFrom.setValue(fromDate);
		dateBoxFrom.setFormat(format);
		dateBoxFrom.setAutoClose(true);
		panelDateInput.add(dateBoxFrom);

		Label lbl = new InlineLabel("~");
		panelDateInput.add(lbl);

		dateBoxTo = new MaterialDatePicker();
		dateBoxTo.addStyleName("date");
		Date toDate = new Date();
		dateBoxTo.setValue(toDate);
		dateBoxTo.setFormat(format);
		dateBoxTo.setAutoClose(true);
		panelDateInput.add(dateBoxTo);

		panelDateInput.setVisible(false);

		btnSearch = new MaterialButton();
		btnSearch.setIconType(IconType.SEARCH);
		btnSearch.setWaves(WavesType.LIGHT);
		btnSearch.addStyleName("search");
		buttonsPanel.add(btnSearch);
		
		btnCloseSearch = new MaterialIcon();
		btnCloseSearch.setIconType(IconType.CLOSE);
		btnCloseSearch.addStyleName("close");
		searchPanel.add(btnCloseSearch);
		
		btnCloseSearch.addClickHandler(e -> {
				textBoxKeyWord.setText("");
				searchPanel.setVisible(false);
			}
		);
	}
	
	public MaterialLink getLblTitle() {
		return this.lblTitle;
	}
	
	public TextBox getTextBoxKeyWord() {
		return this.textBoxKeyWord;
	}

	public MaterialListBox getLisBoxSearchBy() {
		return this.listBoxSearchBy;
	}

	public MaterialButton getBtnSearch() {
		return this.btnSearch;
	}
	
	public FlowPanel getSearchPanel() {
		return this.searchPanel;
	}

	public MaterialButton getBtnRefresh() {
		return this.btnRefresh;
	}

	public MaterialButton getBtnAdd() {
		return this.btnAdd;
	}

	public MaterialButton getBtnRemove() {
		return this.btnRemove;
	}

	public MaterialDatePicker getDateBoxFrom() {
		return this.dateBoxFrom;
	}

	public MaterialDatePicker getDateBoxTo() {
		return this.dateBoxTo;
	}

	public String getDateBoxFromText() {
        Date fromDate = new Date();
        CalendarUtil.addDaysToDate(fromDate, -days);
	    if (this.dateBoxFrom.getValue() == null) {
            return dateFormat.format(fromDate);
        }
		return dateFormat.format(this.dateBoxFrom.getValue());
	}

	public String getDateBoxToText() {
		Date toDate = new Date();
        if (this.dateBoxTo.getValue() == null) {
            return dateFormat.format(toDate);
        }
	    return dateFormat.format(this.dateBoxTo.getValue());
	}

	public void setDayPeriod(int days) {
		this.days = days;

        dateBoxTo.setValue(new Date());
        Date fromDate = new Date();
		CalendarUtil.addDaysToDate(fromDate, -days);
        dateBoxFrom.setValue(fromDate);
	}

	public FlowPanel getPanelDateInput() {
		return this.panelDateInput;
	}

	public void addBtn(MaterialButton iconBtn) {
		buttonsPanel.add(iconBtn);
	}
}
