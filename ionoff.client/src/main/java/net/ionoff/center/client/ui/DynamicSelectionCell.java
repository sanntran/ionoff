/*
 * Copyright 2010 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */
package net.ionoff.center.client.ui;

import com.google.gwt.cell.client.AbstractInputCell;
import com.google.gwt.cell.client.ValueUpdater;
import com.google.gwt.core.client.GWT;
import com.google.gwt.dom.client.Element;
import com.google.gwt.dom.client.NativeEvent;
import com.google.gwt.dom.client.SelectElement;
import com.google.gwt.safehtml.client.SafeHtmlTemplates;
import com.google.gwt.safehtml.shared.SafeHtml;
import com.google.gwt.safehtml.shared.SafeHtmlBuilder;

import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.shared.dto.BaseDto;

import java.util.ArrayList;
import java.util.List;

/**
 * A {@link Cell} used to render a drop-down list.
 */
public class DynamicSelectionCell<T extends BaseDto> extends AbstractInputCell<String, String> {

	interface Template extends SafeHtmlTemplates {
		@Template("<option value=\"{0}\">{0}</option>")
		SafeHtml deselected(String option);

		@Template("<option value=\"{0}\" selected=\"selected\">{0}</option>")
		SafeHtml selected(String option);
	}

	private static Template template;

	private final List<String> options;
	private final List<T> dataObjects;

	/**
	 * Construct a new {@link SelectionCell} with the specified options.
	 *
	 * @param options
	 *            the options in the cell
	 */
	public DynamicSelectionCell() {
		super("change");
		this.dataObjects = new ArrayList<T>();
		if (template == null) {
			template = GWT.create(Template.class);
		}
		this.options = new ArrayList<String>();
	}
	
	public List<T> getDataObjects() {
		return dataObjects;
	}
	
	public void setDataObjects(List<T> newDataObjects) {
		options.clear();
		dataObjects.clear();
		options.add(AdminLocale.getAdminConst().none());
		dataObjects.add(null);
		for (T op : newDataObjects) {
			addOption(op);
		}
	}
	
	public void addOption(T newOp) {
		String option = new String(BaseDto.formatNameID(newOp));
		options.add(option);
		dataObjects.add(newOp);
	}

	public void removeOption(BaseDto op) {
		String option = new String(BaseDto.formatNameID(op));
		options.remove(option);
		dataObjects.remove(op);
	}

	@Override
	public void onBrowserEvent(Context context, Element parent, String value, NativeEvent event,
			ValueUpdater<String> valueUpdater) {
		super.onBrowserEvent(context, parent, value, event, valueUpdater);
		String type = event.getType();
		if ("change".equals(type)) {
			Object key = context.getKey();
			SelectElement select = parent.getFirstChild().cast();
			String newValue = options.get(select.getSelectedIndex());
			setViewData(key, newValue);
			finishEditing(parent, newValue, key, valueUpdater);
			if (valueUpdater != null) {
				valueUpdater.update(newValue);
			}
		}
	}

	@Override
	public void render(Context context, String value, SafeHtmlBuilder sb) {
		// Get the view data.
		Object key = context.getKey();
		String viewData = getViewData(key);
		if (viewData != null && viewData.equals(value)) {
			clearViewData(key);
			viewData = null;
		}

		int selectedIndex = getSelectedIndex(viewData == null ? value : viewData);
		sb.appendHtmlConstant("<select tabindex=\"-1\">");
		int index = 0;
		for (String option : options) {
			if (index++ == selectedIndex) {
				sb.append(template.selected(option));
			}
			else {
				sb.append(template.deselected(option));
			}
		}
		sb.appendHtmlConstant("</select>");
	}

	private int getSelectedIndex(String value) {
		int optionsCount = options.size();
		for (int i = 0; i < optionsCount; i++) {
			if (options.get(i).equals(value)) {
				return i;
			}
		}
		return -1;
	}

	public BaseDto getSelectedObject(String value) {
		int selectedIndex = getSelectedIndex(value);
		if (selectedIndex == -1) {
			return null;
		}
		return dataObjects.get(selectedIndex);
	}
}