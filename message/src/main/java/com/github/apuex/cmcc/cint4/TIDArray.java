/*
 * Copyright (c) 2021-2023 WINCOM.
 * Copyright (c) 2021-2023 Wangxy <xtwxy@hotmail.com>
 *
 * All rights reserved. 
 *
 * This program and the accompanying materials
 * are made available under the terms of the Mozilla Public License 2.0.
 *
 * Contributors:
 *   Wangxy - initial implementation and documentation.
*/

package com.github.apuex.cmcc.cint4;

import java.io.Serializable;
import java.nio.ByteBuffer;
import java.util.LinkedList;
import java.util.List;

/**
 * TID数组
 */
public class TIDArray implements Serializable {
	private static final long serialVersionUID = 1L;

	public TIDArray() {
		
	}
	
	public TIDArray(List<TID> l) {
		this.values.addAll(l);
	}

	public static void encode(ByteBuffer buf, TIDArray v) {
		buf.putInt(v.values.size());
		for (TID e : v.values) {
			TID.encode(buf, e);
		}
	}

	public static TIDArray decode(ByteBuffer buf) {
		TIDArray v = new TIDArray();
		final int size = buf.getInt();
		for (int i = 0; i != size; ++i) {
			v.values.add(TID.decode(buf));
		}
		return v;
	}

  @Override
  public boolean equals(Object o) {
  	TIDArray r = null;
      if(o instanceof TIDArray) {
          r = (TIDArray) o;
      } else {
          return false;
      }

      boolean result =
          ( this.values.equals(r.values)
          );

      return result;
  }

  @Override
  public String toString() {
      StringBuilder builder = new StringBuilder();
      builder
          .append("TIDArray { ")
          .append("values=").append(this.values)
          .append(" }");

      return builder.toString();
  }

  public List<TID> values = new LinkedList<TID>();
}
